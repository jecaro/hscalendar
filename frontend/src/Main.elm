module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, nav, p, section, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Platform.Cmd exposing (batch)
import String exposing (padLeft)
import Task exposing (perform)
import Time
    exposing
        ( Month(..)
        , Posix
        , Weekday(..)
        , Zone
        , here
        , millisToPosix
        , now
        , toDay
        , toHour
        , toMinute
        , toMonth
        , toSecond
        , toYear
        , utc
        )
import Time.Extra exposing (Interval(..), add)


type alias Model =
    { zone : Zone
    , posix : Posix
    }


type Msg
    = AdjustZone Zone
    | AdjustPosix Posix


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
    ( Model utc (millisToPosix 0)
    , batch
        [ perform AdjustZone here
        , perform AdjustPosix now
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustZone zone ->
            ( { model | zone = zone }, Cmd.none )

        AdjustPosix posix ->
            ( { model | posix = posix }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


monthToInt : Month -> Int
monthToInt m =
    case m of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


posixToString : Zone -> Posix -> String
posixToString z t =
    let
        year =
            String.fromInt (toYear z t)

        month =
            padLeft 2 '0' (String.fromInt (monthToInt (toMonth z t)))

        day =
            padLeft 2 '0' (String.fromInt (toDay z t))

        hour =
            padLeft 2 '0' (String.fromInt (toHour z t))

        minute =
            padLeft 2 '0' (String.fromInt (toMinute z t))

        second =
            padLeft 2 '0' (String.fromInt (toSecond z t))
    in
    day ++ "/" ++ month ++ "/" ++ year ++ "-" ++ hour ++ ":" ++ minute ++ ":" ++ second


previousDay : Model -> Posix
previousDay model =
    add Day -1 model.zone model.posix


nextDay : Model -> Posix
nextDay model =
    add Day 1 model.zone model.posix



--<section class="hero">
--  <div class="hero-body">
--    <div class="container">
--      <h1 class="title">
--        Hero title
--      </h1>
--      <h2 class="subtitle">
--        Hero subtitle
--      </h2>
--    </div>
--  </div>
--</section>


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
                            [ button [ class "button", onClick (AdjustPosix (previousDay model)) ] 
                                [ text "Prev" ]
                            ]
                        ]
                    , div [ class "level-item" ]
                        [ p []
                            [ text ("Posix time is " ++ posixToString model.zone model.posix) ]
                        ]
                    , div [ class "level-right" ]
                        [ div [ class "level-item" ]
                            [ button [ class "button", onClick (AdjustPosix (nextDay model)) ] 
                                [ text "Next" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
