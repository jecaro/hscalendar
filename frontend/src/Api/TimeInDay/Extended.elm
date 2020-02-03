module Api.TimeInDay.Extended exposing (toString, fromString)

import Api exposing (TimeInDay(..))

toString : TimeInDay -> String
toString timeInDay = 
  case timeInDay of
     Morning -> "morning"
     Afternoon -> "afternoon"

fromString : String -> Result String TimeInDay
fromString string = 
    case string of
       "Morning" -> Ok Morning
       "morning" -> Ok Morning
       "Afternoon" -> Ok Afternoon
       "afternoon" -> Ok Afternoon
       _ -> Err <| "Error parsing string to TimeInDay " ++ string

