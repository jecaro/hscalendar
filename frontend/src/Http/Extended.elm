-- Code found here:
-- https://github.com/krisajenkins/remotedata/issues/28#issuecomment-511950834


module Http.Extended exposing (errorToString)

import Http 


errorToString : Http.Error -> String
errorToString err =
    case err of
        Http.Timeout ->
            "Timeout exceeded"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus no ->
            "Bad status: " ++ String.fromInt no

        Http.BadBody str ->
            "Unexpected response from api: " ++ str

        Http.BadUrl url ->
            "Malformed url: " ++ url
