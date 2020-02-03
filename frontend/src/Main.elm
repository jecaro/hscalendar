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
import Api.IdleDayType.Extra as IdleDayType exposing (toString)
import Api.Office.Extra as Office exposing (toString)
import Api.TimeInDay.Extra as TimeInDay exposing (fromString, toString)
import TimeOfDay as TimeOfDay exposing (toString)

type alias Response = Maybe (Result Error HalfDay)

type alias Model = 
    { date : Date
    , timeInDay : TimeInDay
    , response : Response
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
      , response = Nothing
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
            let model_ = { model | date = date }
            in ( model_, httpCommand model_ )
        SetTimeInDay timeInDay -> 
            let model_ = { model | timeInDay = timeInDay }
            in ( model_, httpCommand model_ )
        ResponseReceived response -> ( { model | response = Just response}, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


previousDay : Model -> Date
previousDay model = add Days -1 model.date


nextDay : Model -> Date
nextDay model = add Days 1 model.date

toInvertIsoString : Date -> String
toInvertIsoString = format "dd-MM-yyyy"

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
            [ p [ class "subtitle" ] [ text (toIsoString model.date) ]
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

viewResponse : Response -> Html msg
viewResponse response = 
    case response of
        Nothing -> p [] []
        Just (Ok (MkHalfDayWorked worked)) -> viewWorked worked
        Just (Ok (MkHalfDayIdle idle)) -> viewIdle idle
        Just (Err (BadStatus 404)) -> viewNoEntry
        Just (Err _) -> p [] [ text "ERROR"]

viewWorked : Worked -> Html msg
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
            [ div [ class "content" ] [ viewResponse model.response ] ]
        ]
