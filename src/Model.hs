-- | This module reexport the public functions from "Internal.Model"
module Model
    ( EntityField
        ( HalfDayType
        , HalfDayWorkedHalfDayId
        , HalfDayWorkedNotes 
        , HalfDayWorkedOffice
        , HalfDayWorkedProjectId
        , ProjectName
        )
    , HalfDay(..)
    , HalfDayId
    , HalfDayWorked(..)
    , Project
    , ProjName
    , ProjectId
    , Unique(DayAndTimeInDay, UniqueHalfDayId, UniqueName)
    , migrateAll
    , mkProject
    , mkProjectLit
    , projectName
    )

where 

import Internal.Model