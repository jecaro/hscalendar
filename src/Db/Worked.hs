{-# LANGUAGE TemplateHaskell #-}
module Db.Worked
where

import           RIO

import qualified RIO.Time as Time (Day, TimeOfDay)

import           Data.Aeson (FromJSON, ToJSON)
import           Lens.Micro.Platform (makeFields)

import           Db.Notes(Notes)
import           Db.Project(Project)
import           Db.Office (Office)
import           Db.TimeInDay (TimeInDay)

data Worked = MkWorked
    { _workedDay       :: !Time.Day
    , _workedTimeInDay :: !TimeInDay
    , _workedArrived   :: !Time.TimeOfDay
    , _workedLeft      :: !Time.TimeOfDay
    , _workedOffice    :: !Office
    , _workedNotes     :: !Notes
    , _workedProject   :: !Project
    }
    deriving (Eq, Generic, Show)
makeFields ''Worked

instance FromJSON Worked
instance ToJSON Worked


