-- | Functions related to the type 'Worked'
{-# LANGUAGE TemplateHaskell #-}
module Db.Worked
where

import           RIO

import qualified RIO.Text as Text (null)
import qualified RIO.Time as Time (Day, TimeOfDay)
import           RIO.Time.Extended ()

import           Data.Aeson (FromJSON, ToJSON)
import           Lens.Micro.Platform (makeFields)

import           Db.Notes (Notes, unNotes)
import           Db.Project (Project)
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

instance Display Worked where
    display worked
        =  "\t\t" <> display (worked ^. office)
            <> ": "
            <> display (worked ^. arrived)
            <> " - "
            <> display (worked ^. left)
            <> "\n"
        <> "\t\tProject: " <> display (worked ^. project) <> "\n"
        <> "\t\tNotes:" <> eolAfterNoteLabel
        <> display (worked ^. notes)
      where eolAfterNoteLabel
              | Text.null (unNotes $ worked ^. notes) = ""
              | otherwise = "\n"
