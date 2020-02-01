module Main exposing (main)

import Browser
import Html exposing (Html,  div, text)
import Html.Attributes exposing (class)


type alias Msg = ()


type alias Model = Int


main : Program Int Model Msg
main =
    Browser.element 
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view 
        }


init : Int -> (Model, Cmd Msg)
init i = (i, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update _ model =
    (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

view : Int -> Html Msg
view i =
    div [ class "container" ]
        [ text ("Current Posix time is " ++ String.fromInt i)
        ]
