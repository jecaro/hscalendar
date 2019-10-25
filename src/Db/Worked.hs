-- | Functions related to the type 'Worked'
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

-- | Data associated with a half-day worked
data Worked = MkWorked
    { _workedDay       :: !Time.Day       -- ^ The day
    , _workedTimeInDay :: !TimeInDay      -- ^ Morning/Afternoon ?
    , _workedArrived   :: !Time.TimeOfDay -- ^ Arrival time
    , _workedLeft      :: !Time.TimeOfDay -- ^ Departure time
    , _workedOffice    :: !Office         -- ^ Which office
    , _workedNotes     :: !Notes          -- ^ Some notes
    , _workedProject   :: !Project        -- ^ And the project
    }
    deriving (Eq, Generic, Show)
makeFields ''Worked

instance FromJSON Worked
instance ToJSON Worked


