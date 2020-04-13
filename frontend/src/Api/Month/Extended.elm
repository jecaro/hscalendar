module Api.Month.Extended exposing (fromDate, next, previous, toHumanString, toString)

import Api
import Date
import String


toString : Api.Month -> String
toString { year, month } =
    String.fromInt year ++ "-" ++ (String.pad 2 '0' <| String.fromInt month)


toHumanString : Api.Month -> String
toHumanString { year, month } =
    let
        name =
            case month of
                1 ->
                    "January"

                2 ->
                    "February"

                3 ->
                    "March"

                4 ->
                    "April"

                5 ->
                    "May"

                6 ->
                    "June"

                7 ->
                    "July"

                8 ->
                    "August"

                9 ->
                    "September"

                10 ->
                    "October"

                11 ->
                    "November"

                12 ->
                    "December"

                _ ->
                    "Unknown"
    in
    name ++ " " ++ String.fromInt year


fromDate : Date.Date -> Api.Month
fromDate date =
    Api.Month (Date.year date) (Date.monthNumber date)


next : Api.Month -> Api.Month
next { year, month } =
    case month + 1 of
        13 ->
            { year = year + 1, month = 1 }

        _ ->
            { year = year, month = month + 1 }


previous : Api.Month -> Api.Month
previous { year, month } =
    case month - 1 of
        0 ->
            { year = year - 1, month = 12 }

        _ ->
            { year = year, month = month - 1 }
