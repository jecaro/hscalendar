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
import Html exposing (Html, button, div, h1, nav, p, section, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http exposing (Error, get, expectString)
import Task exposing (perform)
import Time exposing (Month(..))


type alias Model = 
    { date : Date
    , response : Maybe (Result Error String)
    }


type Msg
    = SetDate Date
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
      , response = Nothing
      }
    , perform SetDate today
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetDate date ->
              let 
                httpCommand = get
                    { url = "/diary/" ++ toInvertIsoString date ++ "/morning"
                    , expect = expectString ResponseReceived 
                    }
              in ( { model | date = date }, httpCommand )
        ResponseReceived response -> ({ model | response = Just response}, Cmd.none )


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
        [ div [ class "level-left" ]
            [ div [ class "level-item" ]
                [ button [ class "button", onClick (SetDate (previousDay model)) ] 
                    [ text "Prev" ]
                ]
            ]
        , div [ class "level-item" ]
            [ p []
                [ text (toIsoString model.date) ]
            ]
        , div [ class "level-right" ]
            [ div [ class "level-item" ]
                [ button [ class "button", onClick (SetDate (nextDay model)) ] 
                    [ text "Next" ]
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
