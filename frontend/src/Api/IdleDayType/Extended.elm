module Api.IdleDayType.Extended exposing (fromString, toString)

import Api exposing (IdleDayType(..))

toString : IdleDayType -> String
toString dayType = 
    case dayType of
       PaidLeave -> "Paid leave"
       FamilyEvent -> "Family event"
       RTTE -> "RTTE"
       RTTS -> "RTTS"
       UnpaidLeave -> "Unpaid leave"
       PublicHoliday -> "Public holiday"
       PartTime -> "Part time"

fromString : String -> Result String IdleDayType
fromString str = 
    case str of
       "Paid leave" -> Ok PaidLeave
       "Family event" -> Ok FamilyEvent
       "RTTE" -> Ok RTTE
       "RTTS" -> Ok RTTS
       "Unpaid leave" -> Ok UnpaidLeave
       "Public holiday" -> Ok PublicHoliday
       "Part time" -> Ok PartTime
       _ -> Err "Bad string"    