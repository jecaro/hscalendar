module Api.HalfDay.Extended exposing (setIdleDayType, setWorkOption)

import Api
    exposing
        ( HalfDay(..)
        , IdleDayType
        , SetArrived(..)
        , SetLeft(..)
        , SetNotes(..)
        , SetOffice(..)
        , SetProj(..)
        , WorkOption(..)
        )


setIdleDayType : IdleDayType -> HalfDay -> HalfDay
setIdleDayType idleDayType halfDay =
    case halfDay of
        (MkHalfDayWorked _) as worked ->
            worked

        MkHalfDayIdle idle ->
            MkHalfDayIdle { idle | idleDayType = idleDayType }


setWorkOption : WorkOption -> HalfDay -> HalfDay
setWorkOption workOption halfDay =
    case halfDay of
        MkHalfDayWorked worked ->
            case workOption of
                MkSetArrived (SetArrived arrived) ->
                    MkHalfDayWorked { worked | workedArrived = arrived }

                MkSetLeft (SetLeft left) ->
                    MkHalfDayWorked { worked | workedLeft = left }

                MkSetNotes (SetNotes notes) ->
                    MkHalfDayWorked { worked | workedNotes = notes }

                MkSetOffice (SetOffice office) ->
                    MkHalfDayWorked { worked | workedOffice = office }

                MkSetProj (SetProj project) ->
                    MkHalfDayWorked { worked | workedProject = project }

        (MkHalfDayIdle _) as idle ->
            idle
