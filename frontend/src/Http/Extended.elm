-- Code found here:
-- https://github.com/krisajenkins/remotedata/issues/28#issuecomment-511950834


module Http.Extended exposing (errorToString)

import Http exposing (Error(..))


errorToString : Http.Error -> String
errorToString err =
    case err of
        Timeout ->
            "Timeout exceeded"

        NetworkError ->
            "Network error"

        BadStatus no ->
            "Bad status: " ++ String.fromInt no

        BadBody str ->
            "Unexpected response from api: " ++ str

        BadUrl url ->
            "Malformed url: " ++ url
