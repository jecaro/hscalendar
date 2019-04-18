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