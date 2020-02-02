module Main exposing (main)

import Browser
import Date exposing (Date, Unit(..), add, fromCalendarDate, today, toIsoString)
import Html exposing (Html, button, div, h1, nav, p, section, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Task exposing (perform)
import Time exposing (Month(..))


type alias Model = Date


type Msg
    = SetDate Date


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
    ( fromCalendarDate 2019 Jan 1
    , perform SetDate today
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        SetDate date ->
            ( date, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


previousDay : Model -> Date
previousDay = add Days -1 


nextDay : Model -> Date
nextDay = add Days 1 


view : Model -> Html Msg
view model =
    div []
        [ section [ class "hero", class "is-primary" ]
            [ div [ class "hero-body" ]
                [ div [ class "container" ]
                    [ h1 [ class "title" ]
                        [ text "HSCalendar"
                        ]
                    ]
                ]
            ]
        , section [ class "section" ]
            [ div [ class "container" ]
                [ nav [ class "level" ]
                    [ div [ class "level-left" ]
                        [ div [ class "level-item" ]
                            [ button [ class "button", onClick (SetDate (previousDay model)) ] 
                                [ text "Prev" ]
                            ]
                        ]
                    , div [ class "level-item" ]
                        [ p []
                            [ text (toIsoString model) ]
                        ]
                    , div [ class "level-right" ]
                        [ div [ class "level-item" ]
                            [ button [ class "button", onClick (SetDate (nextDay model)) ] 
                                [ text "Next" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
