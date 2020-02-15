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
    , input
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
import Html.Attributes exposing (class, selected, type_, value)
import Html.Events exposing (onClick, onDoubleClick, onInput)
import Html.Events.Extended exposing (onEnter)
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
    , SetArrived(..)
    , SetLeft(..)
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
import Api.TimeOfDay as TimeOfDay exposing (TimeOfDay, fromString, toString)
import Api.WorkOption as WorkOption exposing (encoder)

type Mode 
    = View
    | EditOffice
    | EditProject
    | EditNotes String
    | EditIdleDayType
    | EditArrived
    | EditLeft


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

diaryUrl : Date -> TimeInDay -> String
diaryUrl date timeInDay 
    = "/diary/" 
    ++ toInvertIsoString date 
    ++ "/" 
    ++ TimeInDay.toString timeInDay

idleUrl : Date -> TimeInDay -> String
idleUrl date timeInDay
    = "/diary/idle/" 
    ++ toInvertIsoString date 
    ++ "/" 
    ++ TimeInDay.toString timeInDay

sendGetHalfDay : Date -> TimeInDay -> Cmd Msg
sendGetHalfDay date timeInDay = 
    get
        { url = diaryUrl date timeInDay
        , expect = Http.expectJson (fromResult >> HalfDayResponse) HalfDay.decoder
        }

sendGetProjects : Cmd Msg
sendGetProjects = 
    get
        { url = "/project/"
        , expect = Http.expectJson (fromResult >> ProjectsResponse) (Decode.list Project.decoder)
        }

sendSetOffice : Date -> TimeInDay -> Office -> Cmd Msg
sendSetOffice date timeInDay office = 
    let
        workOption = MkSetOffice <| SetOffice office
    in
        sendSetWorkOption date timeInDay workOption


sendSetNotes : Date -> TimeInDay -> String -> Cmd Msg
sendSetNotes date timeInDay notes = 
    let
        workOption = MkSetNotes <| SetNotes <| Notes notes
    in
        sendSetWorkOption date timeInDay workOption


sendSetProject : Date -> TimeInDay -> String -> Cmd Msg
sendSetProject date timeInDay project = 
    let
        workOption = MkSetProj <| SetProj <| Project project
    in
        sendSetWorkOption date timeInDay workOption


sendSetArrived : Date -> TimeInDay -> TimeOfDay -> Cmd Msg
sendSetArrived date timeInDay timeOfDay = 
    let
        workOption = MkSetArrived <| SetArrived timeOfDay
    in
        sendSetWorkOption date timeInDay workOption


sendSetLeft : Date -> TimeInDay -> TimeOfDay -> Cmd Msg
sendSetLeft date timeInDay timeOfDay = 
    let
        workOption = MkSetLeft <| SetLeft timeOfDay
    in
        sendSetWorkOption date timeInDay workOption


sendSetWorkOption : Date -> TimeInDay -> WorkOption -> Cmd Msg
sendSetWorkOption date timeInDay option = 
    request
        { method = "PUT"
        , headers = []
        , url = diaryUrl date timeInDay
        , body = jsonBody <| Encode.list WorkOption.encoder [option]
        , expect = expectWhatever <| EditResponse << RemoteData.map (always (setWorkOption option)) << fromResult
        , timeout = Nothing
        , tracker = Nothing
        }

sendSetIdleDayType : Date -> TimeInDay -> IdleDayType -> Cmd Msg
sendSetIdleDayType date timeInDay idleDayType = 
        request
            { method = "PUT"
            , headers = []
            , url = idleUrl date timeInDay
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
            in ( model_, sendGetHalfDay model_.date model_.timeInDay )
        
        SetTimeInDay timeInDay -> 
            let model_ = { model | timeInDay = timeInDay, halfDay = Loading, mode = View  }
            in ( model_, sendGetHalfDay model_.date model_.timeInDay )
        
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
    case model.projects of
        NotAsked -> p [] []
        Loading -> p [] [ text "Loading projects ..." ]
        Failure _ -> p [] [ text "Error loading projects"]
        Success projects -> 
            case model.halfDay of
                NotAsked -> p [] []
                Loading -> p [] [ text "Loading ..." ]
                Failure (BadStatus 404) -> viewNoEntry
                Failure _ -> p [] [ text "Error loading half-day"]
                Success (MkHalfDayWorked worked) -> 
                    viewWorked model.date model.timeInDay model.mode projects worked
                Success (MkHalfDayIdle idle) -> 
                    viewIdle model.date model.timeInDay model.mode idle


officeSelect : Date -> TimeInDay -> Office -> Html Msg
officeSelect date timeInDay current = 
    let
        toOption office = 
            option [ selected <| office == current ] 
                [ text <| Office.toString office ]
        setEditHalfDay = 
            SetEditHalfDay 
                << sendSetOffice date timeInDay 
                << withDefault Rennes 
                << Office.fromString 
    in
        div [ class "field" ]
            [ div [ class "control" ]
                [ div 
                    [ class "select"
                    , onInput <| setEditHalfDay
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

projectSelect : Date -> TimeInDay -> List Project -> Project -> Html Msg
projectSelect date timeInDay projects current = 
    let
        toOption project = 
            option [ selected <| project == current ] 
                [ text project.unProject]
    in
        div [ class "field" ]
            [ div [ class "control" ]
                [ div 
                    [ class "select"
                    , onInput <| SetEditHalfDay << sendSetProject date timeInDay
                    ]
                    [ select [] <| List.map toOption projects ]
                ]
            ]


idleDayTypeSelect : Date -> TimeInDay -> IdleDayType -> Html Msg
idleDayTypeSelect date timeInDay current = 
    let
        toOption idleDayType = 
            option [ selected <| idleDayType == current ] 
                [ text <| IdleDayType.toString idleDayType ]
        setEditHalfDay =
            SetEditHalfDay 
                << sendSetIdleDayType date timeInDay 
                << withDefault PaidLeave 
                << IdleDayType.fromString 
    in
        div [ class "field" ]
            [ div [ class "control" ]
                [ div 
                    [ class "select"
                    , onInput <| setEditHalfDay
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

arrivedInput : Date -> TimeInDay -> TimeOfDay -> Html Msg
arrivedInput date timeInDay timeOfDay = 
    let
        setEditHalfDay =
            SetEditHalfDay 
                << sendSetArrived date timeInDay 
                << withDefault timeOfDay
                << TimeOfDay.fromString 
    in
        div [ class "field", class "is-inline-block" ]
            [ div [ class "control" ]
                [ input 
                    [ class "input", type_ "text"
                    , value <| TimeOfDay.toString timeOfDay 
                    , onEnter setEditHalfDay
                    ] [ ]
                ]
            ]

leftInput : Date -> TimeInDay -> TimeOfDay -> Html Msg
leftInput date timeInDay timeOfDay = 
    let
        setEditHalfDay =
            SetEditHalfDay 
                << sendSetLeft date timeInDay 
                << withDefault timeOfDay
                << TimeOfDay.fromString 
    in
        div [ class "field", class "is-inline-block" ]
            [ div [ class "control" ]
                [ input 
                    [ class "input", type_ "text"
                    , value <| TimeOfDay.toString timeOfDay 
                    , onEnter setEditHalfDay
                    ] [ ]
                ]
            ]

viewWorked : Date -> TimeInDay -> Mode -> List Project -> Worked -> Html Msg
viewWorked date timeInDay mode projects 
    { workedArrived
    , workedLeft
    , workedOffice
    , workedNotes
    , workedProject } = 
    let
        cellOffice = 
            case mode of
                EditOffice ->
                    th [] [ officeSelect date timeInDay workedOffice ]
                _ -> 
                    th [ onDoubleClick <| SetMode EditOffice ] 
                        [ text <| Office.toString workedOffice ]
        cellNotes = 
            case mode of
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
                                    , onClick 
                                        <| SetEditHalfDay 
                                        <| sendSetNotes date timeInDay notes 
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
            case mode of
                EditProject -> 
                    th [] [ projectSelect date timeInDay projects workedProject ]
                _ ->
                    th [ onDoubleClick <| SetMode EditProject ] 
                        [ text <| workedProject.unProject ]
        divArrived =
            case mode of
                EditArrived ->
                    arrivedInput date timeInDay workedArrived
                _ -> 
                    div [ class "is-inline-block"
                        , onDoubleClick <| SetMode EditArrived ]
                        [ text <| TimeOfDay.toString workedArrived ]
        divLeft =
            case mode of
                EditLeft ->
                    leftInput date timeInDay workedLeft
                _ -> 
                    div [ class "is-inline-block"
                        , onDoubleClick <| SetMode EditLeft ]
                        [ text <| TimeOfDay.toString workedLeft ]
    in
        table [ class "table" ]
            [ tbody []
                [ tr []
                    [ cellOffice
                    , td [] 
                        [ divArrived 
                        , div [ class "is-inline-block" ] 
                            [ text "\u{00A0}-\u{00A0}" ]
                        , divLeft
                        ]
                    ]
                , tr []
                    [ cellProject
                    , cellNotes
                    ]
                ]
            ]
    

viewIdle : Date -> TimeInDay -> Mode -> Idle -> Html Msg
viewIdle date timeInDay mode { idleDayType } = 
    let
        cellIdleDayType = 
            case mode of
                EditIdleDayType -> 
                    th [] [ idleDayTypeSelect date timeInDay idleDayType ]
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
