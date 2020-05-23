{-# LANGUAGE TemplateHaskell #-}

-- | Functions related to the type 'Worked'
module Db.Worked where

import Data.Aeson (FromJSON, ToJSON)
import Db.Notes (Notes, unNotes)
import Db.Office (Office)
import Db.Project (Project)
import Db.TimeInDay (TimeInDay)
import Lens.Micro.Platform (makeFields)
import RIO
import qualified RIO.Text as Text (null)
import qualified RIO.Time as Time (Day, TimeOfDay)
import RIO.Time.Extended ()

-- | Data associated with a half-day worked
data Worked = MkWorked
    { -- | The day
      _workedDay :: !Time.Day,
      -- | Morning/Afternoon ?
      _workedTimeInDay :: !TimeInDay,
      -- | Arrival time
      _workedArrived :: !Time.TimeOfDay,
      -- | Departure time
      _workedLeft :: !Time.TimeOfDay,
      -- | Which office
      _workedOffice :: !Office,
      -- | Some notes
      _workedNotes :: !Notes,
      -- | And the project
      _workedProject :: !Project
    }
    deriving (Eq, Generic, Show)

makeFields ''Worked

instance FromJSON Worked

instance ToJSON Worked

instance Display Worked where
    display worked =
        "\t\t" <> display (worked ^. office)
            <> ": "
            <> display (worked ^. arrived)
            <> " - "
            <> display (worked ^. left)
            <> "\n"
            <> "\t\tProject: "
            <> display (worked ^. project)
            <> "\n"
            <> "\t\tNotes:"
            <> eolAfterNoteLabel
            <> display (worked ^. notes)
        where
            eolAfterNoteLabel
                | Text.null (unNotes $ worked ^. notes) = ""
                | otherwise = "\n"
