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
import Json.Encode exposing (list)
import RemoteData exposing (RemoteData(..), WebData, fromResult)
import Task exposing (perform)
import Time exposing (Month(..))

import Api as Api exposing 
    ( HalfDay(..)
    , Idle
    , Office(..)
    , SetOffice(..)
    , TimeInDay(..)
    , WorkOption(..)
    , Worked
    )
import Api.HalfDay as HalfDay exposing (decoder)
import Api.IdleDayType.Extended as IdleDayType exposing (toString)
import Api.Office.Extended as Office exposing (toString)
import Api.TimeInDay.Extended as TimeInDay exposing (fromString, toString)
import Api.TimeOfDay as TimeOfDay exposing (toString)
import Api.WorkOption as WorkOption exposing (encoder)

type Mode 
    = View
    | EditOffice


type alias Model = 
    { date : Date
    , timeInDay : TimeInDay
    , halfDay : WebData HalfDay
    , edit : WebData WorkOption
    , mode : Mode
    }


type Msg
    = SetDate Date
    | SetTimeInDay TimeInDay
    | HalfDayResponse (WebData HalfDay)
    | EditResponse (WebData WorkOption)
    | SetMode Mode
    | SetOffice Office


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
      , mode = View
      , edit = NotAsked
      }
    , perform SetDate today
    )

diaryUrl : Model -> String
diaryUrl model 
    = "/diary/" 
    ++ toInvertIsoString model.date 
    ++ "/" 
    ++ TimeInDay.toString model.timeInDay

sendGetHalfDay : Model -> Cmd Msg
sendGetHalfDay model = 
    get
        { url = diaryUrl model
        , expect = Http.expectJson (fromResult >> HalfDayResponse) HalfDay.decoder
        }

sendSetOffice : Model -> Office -> Cmd Msg
sendSetOffice model office = 
    let
        workOption = MkSetOffice <| Api.SetOffice office
    in
        request
            { method = "PUT"
            , headers = []
            , url = diaryUrl model
            , body = jsonBody <| list WorkOption.encoder [workOption]
            , expect = expectWhatever <| EditResponse << RemoteData.map (always workOption) << fromResult
            , timeout = Nothing
            , tracker = Nothing
            }

applyWorkOption : WorkOption -> HalfDay -> HalfDay
applyWorkOption workOption halfDay = 
    case (workOption, halfDay) of
        (MkSetOffice (Api.SetOffice office), MkHalfDayWorked worked) -> 
            MkHalfDayWorked { worked | workedOffice = office}
        _ -> halfDay

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
            ( { model | halfDay = response, mode = View  }, Cmd.none )

        EditResponse response -> 
            ( { model | edit = response
              , halfDay = RemoteData.map2 applyWorkOption response model.halfDay
              , mode = View }
            , Cmd.none
            )

        SetMode mode ->
            ( { model | mode = mode }, Cmd.none )

        SetOffice office -> 
            let model_ = { model | edit = Loading }
            in ( model_, sendSetOffice model_ office )


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
                    [ div [ class "select", onInput <| SetTimeInDay << Result.withDefault Morning << fromString ] 
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
        Success (MkHalfDayWorked worked) -> viewWorked worked model.mode 
        Success (MkHalfDayIdle idle) -> viewIdle idle
        Failure (BadStatus 404) -> viewNoEntry
        Failure _ -> p [] [ text "ERROR"]


officeSelect : Office -> Html Msg
officeSelect current = 
    let
        setOption office = 
            option [ selected <| office == current ] [ text <| Office.toString office ]
    in
        div [ class "field" ]
            [ div [ class "control" ]
                [ div [ class "select", onInput <| SetOffice << Result.withDefault Rennes << Office.fromString ]
                    [ select [] 
                        [ setOption Rennes
                        , setOption Home
                        , setOption Poool
                        , setOption OutOfOffice
                        ]
                    ]
                ]
            ]

viewWorked : Worked -> Mode -> Html Msg
viewWorked 
    { workedArrived
    , workedLeft
    , workedOffice
    , workedNotes
    , workedProject } mode = 
    let
        cellOffice = 
            case mode of
                View -> 
                    th [ onDoubleClick <| SetMode EditOffice ] [ text <| Office.toString workedOffice ]
                EditOffice -> 
                    th [] [ officeSelect workedOffice ]
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
                    [ th [] [ text <| workedProject.unProject ]
                    , td [] [ text <| workedNotes.unNotes ]
                    ]
                ]
            ]
    

viewIdle : Idle -> Html msg
viewIdle { idleDayType } = 
    table [ class "table" ]
        [ tbody []
            [ tr []
                [ th [] [ text <| IdleDayType.toString idleDayType ]
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
