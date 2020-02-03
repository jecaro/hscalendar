module Api.IdleDayType.Extended exposing (toString)

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
