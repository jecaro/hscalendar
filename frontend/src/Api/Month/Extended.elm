module Api.Month.Extended exposing (fromDate, next, previous, toString)

import Api 
import Date 
import String


toString : Api.Month -> String
toString { year, month } =
    String.fromInt year ++ "-" ++ (String.pad 2 '0' <| String.fromInt month)


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
