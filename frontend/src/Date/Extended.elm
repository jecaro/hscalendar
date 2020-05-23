-- Code found here: https://github.com/folq/haskell-to-elm/issues/2


module Date.Extended exposing (decoder, encode, toStringWithWeekday)

import Date
import Json.Decode
import Json.Encode


encode : Date.Date -> Json.Encode.Value
encode =
    Date.toIsoString >> Json.Encode.string


decoder : Json.Decode.Decoder Date.Date
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


toStringWithWeekday : Date.Date -> String
toStringWithWeekday date =
    Date.toIsoString date ++ " " ++ Date.format "EEEE" date
