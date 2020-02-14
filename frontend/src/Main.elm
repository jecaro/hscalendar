module Main exposing (main)

import Browser
import Date exposing 
    ( Date
    , Unit(..)
    , add
    , format
    , fromCalendarDate
    , today
    , toIsoString
    )
import Html exposing 
    ( Html
    , button
    , div
    , h1
    , nav
    , option
    , p
    , section
    , select
    , table
    , tbody
    , textarea
    , td
    , th
    , tr
    , text
    )
import Html.Attributes exposing (class, selected)
import Html.Events exposing (onClick, onDoubleClick, onInput)
import Http exposing 
    ( Error(..)
    , expectJson
    , expectWhatever
    , get
    , jsonBody
    , request
    )
import Json.Decode as Decode exposing (list)
import Json.Encode as Encode exposing (list)
import Platform.Cmd exposing (batch)
import Result exposing (withDefault)
import RemoteData exposing (RemoteData(..), WebData, fromResult)
import Task exposing (perform)
import Time exposing (Month(..))

import Api exposing 
    ( HalfDay(..)
    , Idle
    , IdleDayType(..)
    , Notes
    , Office(..)
    , Project
    , SetNotes(..)
    , SetOffice(..)
    , SetProj(..)
    , TimeInDay(..)
    , WorkOption(..)
    , Worked
    )
import Api.HalfDay as HalfDay exposing (decoder)
import Api.HalfDay.Extended exposing (setIdleDayType, setWorkOption)
import Api.IdleDayType as IdleDayType exposing (encoder)
import Api.IdleDayType.Extended as IdleDayType exposing (fromString, toString)
import Api.Office.Extended as Office exposing (toString)
import Api.Project as Project exposing (decoder)
import Api.TimeInDay.Extended as TimeInDay exposing (fromString, toString)
import Api.TimeOfDay as TimeOfDay exposing (toString)
import Api.WorkOption as WorkOption exposing (encoder)

type Mode 
    = View
    | EditOffice
    | EditProject
    | EditNotes String
    | EditIdleDayType


type alias EditHalfDay = HalfDay -> HalfDay

type alias Model = 
    { date : Date
    , timeInDay : TimeInDay
    , halfDay : WebData HalfDay
    , projects : WebData (List Project)
    , edit : WebData EditHalfDay
    , mode : Mode
    }


type Msg
    = SetDate Date
    | SetTimeInDay TimeInDay
    | HalfDayResponse (WebData HalfDay)
    | EditResponse (WebData EditHalfDay)
    | ProjectsResponse (WebData (List Project))
    | SetMode Mode
    | SetEditHalfDay (Cmd Msg)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { date = fromCalendarDate 2020 Jan 1
      , timeInDay = Morning
      , halfDay = NotAsked
      , projects = NotAsked
      , mode = View
      , edit = NotAsked
      }
    , batch [ perform SetDate today, sendGetProjects ]
    )

diaryUrl : Model -> String
diaryUrl model 
    = "/diary/" 
    ++ toInvertIsoString model.date 
    ++ "/" 
    ++ TimeInDay.toString model.timeInDay

idleUrl : Model -> String
idleUrl model 
    = "/diary/idle/" 
    ++ toInvertIsoString model.date 
    ++ "/" 
    ++ TimeInDay.toString model.timeInDay

sendGetHalfDay : Model -> Cmd Msg
sendGetHalfDay model = 
    get
        { url = diaryUrl model
        , expect = Http.expectJson (fromResult >> HalfDayResponse) HalfDay.decoder
        }

sendGetProjects : Cmd Msg
sendGetProjects = 
    get
        { url = "/project/"
        , expect = Http.expectJson (fromResult >> ProjectsResponse) (Decode.list Project.decoder)
        }

sendSetOffice : Model -> Office -> Cmd Msg
sendSetOffice model office = 
    let
        workOption = MkSetOffice <| SetOffice office
    in
        sendSetWorkOption model workOption


sendSetNotes : Model -> String -> Cmd Msg
sendSetNotes model notes = 
    let
        workOption = MkSetNotes <| SetNotes <| Notes notes
    in
        sendSetWorkOption model workOption


sendSetProject : Model -> String -> Cmd Msg
sendSetProject model project = 
    let
        workOption = MkSetProj <| SetProj <| Project project
    in
        sendSetWorkOption model workOption


sendSetWorkOption : Model -> WorkOption -> Cmd Msg
sendSetWorkOption model option = 
    request
        { method = "PUT"
        , headers = []
        , url = diaryUrl model
        , body = jsonBody <| Encode.list WorkOption.encoder [option]
        , expect = expectWhatever <| EditResponse << RemoteData.map (always (setWorkOption option)) << fromResult
        , timeout = Nothing
        , tracker = Nothing
        }

sendSetIdleDayType : Model -> IdleDayType -> Cmd Msg
sendSetIdleDayType model idleDayType = 
        request
            { method = "PUT"
            , headers = []
            , url = idleUrl model
            , body = jsonBody <| IdleDayType.encoder idleDayType
            , expect = expectWhatever <| EditResponse << RemoteData.map (always (setIdleDayType idleDayType)) << fromResult
            , timeout = Nothing
            , tracker = Nothing
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        SetDate date -> 
            let model_ = { model | date = date, halfDay = Loading, mode = View  }
            in ( model_, sendGetHalfDay model_ )
        
        SetTimeInDay timeInDay -> 
            let model_ = { model | timeInDay = timeInDay, halfDay = Loading, mode = View  }
            in ( model_, sendGetHalfDay model_ )
        
        HalfDayResponse response -> 
            ( { model | halfDay = response, mode = View }, Cmd.none )

        ProjectsResponse response -> 
            ( { model | projects = response }, Cmd.none )

        EditResponse response -> 
            ( { model | edit = response
              , halfDay = RemoteData.map2 identity response model.halfDay
              , mode = View }
            , Cmd.none
            )

        SetMode mode ->
            ( { model | mode = mode }, Cmd.none )

        SetEditHalfDay request -> 
            let model_ = { model | edit = Loading }
            in ( model_, request )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


previousDay : Model -> Date
previousDay model = add Days -1 model.date


nextDay : Model -> Date
nextDay model = add Days 1 model.date

toInvertIsoString : Date -> String
toInvertIsoString = format "dd-MM-yyyy"

weekdayString : Date -> String
weekdayString = format "EEEE"

viewHero : Html msg
viewHero =
    section [ class "hero", class "is-primary" ]
        [ div [ class "hero-body" ]
            [ div [ class "container" ]
                [ h1 [ class "title" ]
                    [ text "HSCalendar"
                    ]
                ]
            ]
        ]

viewNav : Model -> Html Msg
viewNav model = 
    nav [ class "level" ]
        [ div [ class "level-item" ]
            [ button [ class "button", onClick <| SetDate <| previousDay model ] 
                [ text "Prev" ]
            , button [ class "button", onClick <| SetDate <| nextDay model ] 
                [ text "Next" ]                    
            ]
        , div [ class "level-item" ]
            [ p [ class "subtitle" ] 
                [ text <| toIsoString model.date ++ " " ++ weekdayString model.date]
            ]
        , div [ class "level-item" ]
            [ div [ class "field" ]
                [ div [ class "control" ]
                    [ div 
                        [ class "select"
                        , onInput <| SetTimeInDay << withDefault Morning << TimeInDay.fromString 
                        ] 
                        [ select [] 
                            [ option [] [ text "Morning" ]
                            , option [] [ text "Afternoon" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

viewStatus : Model -> Html Msg
viewStatus model = 
    case model.halfDay of
        NotAsked -> p [] [ ]
        Loading -> p [] [ text "Loading ..." ]
        Success (MkHalfDayWorked worked) -> viewWorked model worked  
        Success (MkHalfDayIdle idle) -> viewIdle model idle
        Failure (BadStatus 404) -> viewNoEntry
        Failure _ -> p [] [ text "ERROR"]


officeSelect : Model -> Office -> Html Msg
officeSelect model current = 
    let
        toOption office = 
            option [ selected <| office == current ] [ text <| Office.toString office ]
    in
        div [ class "field" ]
            [ div [ class "control" ]
                [ div 
                    [ class "select"
                    , onInput <| SetEditHalfDay << sendSetOffice model << withDefault Rennes << Office.fromString 
                    ]
                    [ select [] 
                        [ toOption Rennes
                        , toOption Home
                        , toOption Poool
                        , toOption OutOfOffice
                        ]
                    ]
                ]
            ]

projectSelect : Model -> Project -> Html Msg
projectSelect model current = 
    let
        toOption project = 
            option [ selected <| project == current ] 
                [ text project.unProject]
    in
        case model.projects of
            NotAsked -> Debug.todo "TODO"
            Loading -> Debug.todo "TODO"
            Failure _ -> Debug.todo "TODO"
            Success projects ->
                div [ class "field" ]
                    [ div [ class "control" ]
                        [ div 
                            [ class "select" 
                            , onInput <| SetEditHalfDay << sendSetProject model
                            ]
                            [ select [] <| List.map toOption projects ]
                        ]
                    ]


idleDayTypeSelect : Model -> IdleDayType -> Html Msg
idleDayTypeSelect model current = 
    let
        toOption idleDayType = 
            option [ selected <| idleDayType == current ] 
                [ text <| IdleDayType.toString idleDayType ]
    in
        div [ class "field" ]
            [ div [ class "control" ]
                [ div 
                    [ class "select"
                    , onInput <| SetEditHalfDay << sendSetIdleDayType model << withDefault PaidLeave << IdleDayType.fromString 
                    ]
                    [ select [] 
                        [ toOption PaidLeave
                        , toOption FamilyEvent
                        , toOption RTTE
                        , toOption RTTS
                        , toOption UnpaidLeave
                        , toOption PublicHoliday
                        , toOption PartTime
                        ]
                    ]
                ]
            ]

viewWorked : Model -> Worked -> Html Msg
viewWorked model 
    { workedArrived
    , workedLeft
    , workedOffice
    , workedNotes
    , workedProject } = 
    let
        cellOffice = 
            case model.mode of
                EditOffice ->
                    th [] [ officeSelect model workedOffice ]
                _ -> 
                    th [ onDoubleClick <| SetMode EditOffice ] 
                        [ text <| Office.toString workedOffice ]
        cellNotes = 
            case model.mode of
                EditNotes notes -> 
                    td [ ] 
                        [ div [ class "field"]
                            [ div [ class "control" ]
                                [ textarea 
                                    [ class "textarea", onInput <| SetMode << EditNotes ]
                                    [ text notes
                                    ]
                                ]
                            ]
                        , div [ class "field"]
                            [ div [ class "control" ]
                               [ button 
                                    [ class "button"
                                    , onClick <| SetEditHalfDay <| sendSetNotes model notes 
                                    ]
                                    [ text "Submit"
                                    ]
                               ]
                            ]
                        ]
                _ -> 
                    td [ onDoubleClick <| SetMode (EditNotes workedNotes.unNotes) ] 
                        [ text <| workedNotes.unNotes ]
        cellProject =
            case model.mode of
                EditProject -> 
                    th [] [ projectSelect model workedProject ]
                _ ->
                    th [ onDoubleClick <| SetMode EditProject ] 
                        [ text <| workedProject.unProject ]
    in
        table [ class "table" ]
            [ tbody []
                [ tr []
                    [ cellOffice
                    , td [] 
                        [ text 
                            <| TimeOfDay.toString workedArrived 
                            ++ "-" ++ TimeOfDay.toString workedLeft]
                    ]
                , tr []
                    [ cellProject
                    , cellNotes
                    ]
                ]
            ]
    

viewIdle : Model -> Idle -> Html Msg
viewIdle model { idleDayType } = 
    let
        cellIdleDayType = 
            case model.mode of
                EditIdleDayType -> 
                    th [] [ idleDayTypeSelect model idleDayType ]
                _ -> 
                    th [ onDoubleClick <| SetMode EditIdleDayType ] 
                        [ text <| IdleDayType.toString idleDayType ]
    in    
        table [ class "table" ]
            [ tbody []
                [ tr []
                    [ cellIdleDayType
                    ]
                ]
            ]

viewNoEntry : Html msg
viewNoEntry  = 
    table [ class "table" ]
        [ tbody []
            [ tr []
                [ th [] [ text "No entry" ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ viewHero
        , section [ class "section" ]
            [ div [ class "container" ] [ viewNav model ] ]
        , section [ class "section" ]
            [ div [ class "content" ] [ viewStatus model ] ]
        ]
