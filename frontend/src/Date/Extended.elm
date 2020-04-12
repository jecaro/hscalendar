-- Code found here: https://github.com/folq/haskell-to-elm/issues/2


module Date.Extended exposing (decoder, encode, toStringWithWeekday)

import Date exposing (Date, format, toIsoString)
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


toStringWithWeekday : Date -> String
toStringWithWeekday date =
    toIsoString date ++ " " ++ format "EEEE" date
