module Api.TimeInDay.Extended exposing (fromString, toString)

import Api 


toString : Api.TimeInDay -> String
toString timeInDay =
    case timeInDay of
        Api.Morning ->
            "Morning"

        Api.Afternoon ->
            "Afternoon"


fromString : String -> Result String Api.TimeInDay
fromString string =
    case string of
        "Morning" ->
            Ok Api.Morning

        "morning" ->
            Ok Api.Morning

        "Afternoon" ->
            Ok Api.Afternoon

        "afternoon" ->
            Ok Api.Afternoon

        _ ->
            Err <| "Error parsing string to TimeInDay " ++ string
