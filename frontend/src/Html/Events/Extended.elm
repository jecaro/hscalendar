module Html.Events.Extended exposing (onEnter)

import Html exposing (Attribute)
import Html.Events exposing (keyCode, on, targetValue)
import Json.Decode as Decode exposing (fail, map2, succeed)



-- | Function adapted from
-- https://stackoverflow.com/questions/40113213/how-to-handle-enter-key-press-in-input-field


onEnter : (String -> msg) -> Attribute msg
onEnter tagger =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed "Enter keypressed"

            else
                Decode.fail "is not enter"

        decodeEnter =
            Decode.andThen isEnter keyCode
    in
    on "keydown" <| Decode.map2 (always tagger) decodeEnter targetValue
