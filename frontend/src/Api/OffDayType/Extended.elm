module Api.OffDayType.Extended exposing (fromString, toString)

import Api 


toString : Api.OffDayType -> String
toString dayType =
    case dayType of
        Api.PaidLeave ->
            "Paid leave"

        Api.FamilyEvent ->
            "Family event"

        Api.RTTE ->
            "RTTE"

        Api.RTTS ->
            "RTTS"

        Api.UnpaidLeave ->
            "Unpaid leave"

        Api.PublicHoliday ->
            "Public holiday"

        Api.PartTime ->
            "Part time"


fromString : String -> Result String Api.OffDayType
fromString str =
    case str of
        "Paid leave" ->
            Ok Api.PaidLeave

        "Family event" ->
            Ok Api.FamilyEvent

        "RTTE" ->
            Ok Api.RTTE

        "RTTS" ->
            Ok Api.RTTS

        "Unpaid leave" ->
            Ok Api.UnpaidLeave

        "Public holiday" ->
            Ok Api.PublicHoliday

        "Part time" ->
            Ok Api.PartTime

        _ ->
            Err "Bad string"
