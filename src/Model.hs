-- | This module reexport the public functions from "Internal.Model"
module Model
    ( EntityField
        ( HalfDayDay
        , HalfDayId
        , HalfDayTimeInDay
        , HalfDayType
        , HalfDayWorkedHalfDayId
        , HalfDayWorkedNotes 
        , HalfDayWorkedOffice
        , HalfDayWorkedProjectId
        , ProjectId
        , ProjectName
        )
    , HalfDay(..)
    , HalfDayId
    , HalfDayWorked(..)
    , NotesText
    , Project
    , ProjName
    , ProjectId
    , Unique(DayAndTimeInDay, UniqueHalfDayId, UniqueName)
    , migrateAll
    , mkNotes
    , mkProject
    , mkProjectLit
    , projectName
    )

where 

import Internal.Model