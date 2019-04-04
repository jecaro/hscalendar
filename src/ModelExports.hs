module ModelExports
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
    , ProjectId
    , Unique(UniqueHalfDayId, UniqueName, DayAndTimeInDay)
    , migrateAll
    , mkProject
    , projectName
    )

where 

import Model