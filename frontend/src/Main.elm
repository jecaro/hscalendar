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
    , text
    )
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error, get, expectString)
import Task exposing (perform)
import Time exposing (Month(..))

import Api exposing (TimeInDay(..))
import Api.TimeInDay.Extra exposing (fromString, toString)

type alias Model = 
    { date : Date
    , timeInDay : TimeInDay
    , response : Maybe (Result Error String)
    }


type Msg
    = SetDate Date
    | SetTimeInDay TimeInDay
    | ResponseReceived (Result Error String)


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
        { url = "/diary/" ++ toInvertIsoString model.date ++ "/" ++ toString model.timeInDay
        , expect = expectString ResponseReceived 
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
            [ button [ class "button", onClick (SetDate (previousDay model)) ] 
                [ text "Prev" ]
            , button [ class "button", onClick (SetDate (nextDay model)) ] 
                [ text "Next" ]                    
            ]
        , div [ class "level-item" ]
            [ p [ class "subtitle" ] [ text (toIsoString model.date) ]
            ]
        , div [ class "level-item" ]
            [ div [ class "field" ]
                [ div [ class "control" ]
                    [ div [ class "select", onInput (\t -> SetTimeInDay (fromString t)) ] 
                        [ select [] 
                            [ option [] [ text "Morning" ]
                            , option [] [ text "Afternoon" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

viewHalfDay : Model -> Html msg
viewHalfDay model = case model.response of
    Nothing -> p [] []
    Just (Ok jsonString) -> p [] [ text jsonString ]
    Just (Err _) -> p [] [ text "ERROR" ]



view : Model -> Html Msg
view model =
    div []
        [ viewHero
        , section [ class "section" ]
            [ div [ class "container" ] [ viewNav model ] ]
        , section [ class "section" ]
            [ div [ class "content" ] [ viewHalfDay model ] ]
        ]
