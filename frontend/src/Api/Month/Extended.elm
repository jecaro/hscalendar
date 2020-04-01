module Api.Month.Extended exposing (fromDate, next, previous, toString)

import Date exposing (Date, monthNumber, year)
import String exposing (fromInt, pad)

import Api exposing (Month)

toString : Month -> String
toString { year, month } = fromInt year ++ "-" ++ (pad 2 '0' <| fromInt month)

fromDate : Date -> Month
fromDate date = Month (year date) (monthNumber date) 

next : Month -> Month
next { year, month } = 
    case month + 1 of 
        13 -> { year = year + 1, month = 1 }
        _ -> { year = year, month = month + 1 }

previous : Month -> Month
previous { year, month } =
    case month - 1 of 
        0 -> { year = year - 1, month = 12 }
        _ -> { year = year, month = month - 1 }
