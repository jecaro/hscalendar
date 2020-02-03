module Api.TimeInDay.Extended exposing (toString, fromString)

import Api exposing (TimeInDay(..))

toString : TimeInDay -> String
toString timeInDay = 
  case timeInDay of
     Morning -> "morning"
     Afternoon -> "afternoon"

fromString : String -> TimeInDay
fromString string = 
    case string of
       "Morning" -> Morning
       "morning" -> Morning
       "Afternoon" -> Afternoon
       "afternoon" -> Afternoon
       _ -> Morning

