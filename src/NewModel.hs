{-# LANGUAGE TemplateHaskell #-}
module NewModel
where

import qualified RIO.Text as Text (Text)
import qualified RIO.Time as Time (Day, TimeOfDay)

import           Lens.Micro.Platform (makeFields)

import           Office (Office)
import           TimeInDay (TimeInDay)

data IdleDayType = PayedLeave
                 | FamilyEvent
                 | RTTE
                 | RTTS
                 | UnpayedLeave
                 | PublicHoliday 
                 | PartTime

data Idle = Idle
    { _idleDay       :: Time.Day
    , _idleTimeInDay :: TimeInDay
    , _idleDayType   :: IdleDayType
    }
makeFields ''Idle

newtype Notes = Notes Text.Text
newtype Project = Project Text.Text

data Worked = Worked
    { _workedDay       :: Time.Day
    , _workedTimeInDay :: TimeInDay
    , _workedArrived   :: Time.TimeOfDay
    , _workedLeft      :: Time.TimeOfDay
    , _workedOffice    :: Office
    , _workedNotes     :: Notes
    , _workedProject   :: Project
    }
makeFields ''Worked

data HalfDay = HalfDayWorked Worked | HalfDayIdle Idle
