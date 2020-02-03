-- Code found here: https://github.com/folq/haskell-to-elm/issues/2
module Date.Extra exposing (decoder, encode)

import Date exposing (Date)
import Json.Decode
import Json.Encode

encode : Date -> Json.Encode.Value
encode =
    Date.toIsoString >> Json.Encode.string


decoder : Json.Decode.Decoder Date
decoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case Date.fromIsoString str of
                    Result.Err e ->
                        Json.Decode.fail e

                    Result.Ok a ->
                        Json.Decode.succeed a
            )