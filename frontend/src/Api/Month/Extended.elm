module Api.Month.Extended exposing (fromDate, toString)

import Date exposing (Date, monthNumber, year)
import String exposing (fromInt, pad)

import Api exposing (Month)

toString : Month -> String
toString { year, month } = fromInt year ++ "-" ++ (pad 2 '0' <| fromInt month)

fromDate : Date -> Month
fromDate date = Month (year date) (monthNumber date) 