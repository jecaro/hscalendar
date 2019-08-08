{-# LANGUAGE TemplateHaskell #-}
module Worked
where

import           RIO

import qualified RIO.Time as Time (Day, TimeOfDay)

import           Lens.Micro.Platform (makeFields)

import           Notes(Notes)
import           Project(Project)
import           Office (Office)
import           TimeInDay (TimeInDay)

data Worked = MkWorked
    { _workedDay       :: Time.Day
    , _workedTimeInDay :: TimeInDay
    , _workedArrived   :: Time.TimeOfDay
    , _workedLeft      :: Time.TimeOfDay
    , _workedOffice    :: Office
    , _workedNotes     :: Notes
    , _workedProject   :: Project
    }
    deriving (Show, Eq)
makeFields ''Worked


