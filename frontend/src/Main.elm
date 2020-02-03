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
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..), get, expectJson)
import Task exposing (perform)
import Time exposing (Month(..))

import Api exposing (HalfDay(..), Idle, TimeInDay(..), Worked)
import Api.HalfDay exposing (decoder)
import Api.IdleDayType.Extended as IdleDayType exposing (toString)
import Api.Office.Extended as Office exposing (toString)
import Api.TimeInDay.Extended as TimeInDay exposing (fromString, toString)
import Api.TimeOfDay as TimeOfDay exposing (toString)

type Status 
    = Loading
    | LoadError Error
    | LoadOk HalfDay

type alias Model = 
    { date : Date
    , timeInDay : TimeInDay
    , status : Status
    }


type Msg
    = SetDate Date
    | SetTimeInDay TimeInDay
    | ResponseReceived (Result Error HalfDay)


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
      , status = Loading
      }
    , perform SetDate today
    )

httpCommand : Model -> Cmd Msg
httpCommand model = 
    get
        { url 
            = "/diary/" 
            ++ toInvertIsoString model.date 
            ++ "/" 
            ++ TimeInDay.toString model.timeInDay
        , expect = Http.expectJson ResponseReceived decoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        SetDate date -> 
            let model_ = { model | date = date, status = Loading }
            in ( model_, httpCommand model_ )
        
        SetTimeInDay timeInDay -> 
            let model_ = { model | timeInDay = timeInDay, status = Loading }
            in ( model_, httpCommand model_ )
        
        ResponseReceived (Ok hd) -> 
            ( { model | status = LoadOk hd}, Cmd.none )
        
        ResponseReceived (Err error) -> 
            ( { model | status = LoadError error}, Cmd.none )


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
                    [ div [ class "select", onInput <| SetTimeInDay << fromString ] 
                        [ select [] 
                            [ option [] [ text "Morning" ]
                            , option [] [ text "Afternoon" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

viewStatus : Status -> Html Msg
viewStatus status = 
    case status of
        Loading -> p [] [ text "Loading ..." ]
        LoadOk (MkHalfDayWorked worked) -> viewWorked worked
        LoadOk (MkHalfDayIdle idle) -> viewIdle idle
        LoadError (BadStatus 404) -> viewNoEntry
        LoadError _ -> p [] [ text "ERROR"]

viewWorked : Worked -> Html Msg
viewWorked 
    { workedArrived
    , workedLeft
    , workedOffice
    , workedNotes
    , workedProject } = 
    table [ class "table" ]
        [ tbody []
            [ tr []
                [ th [] [ text <| Office.toString workedOffice ]
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
            [ div [ class "content" ] [ viewStatus model.status ] ]
        ]
