-- | Functions related to the type 'HalfDay'
module Db.HalfDay (HalfDay(..), day, displayHdWithDate, timeInDay)
where

import           RIO
import qualified RIO.Time as Time (Day)

import           Data.Aeson (FromJSON, ToJSON)

import qualified Db.Idle as Idle (Idle(..), day, timeInDay)
import           Db.TimeInDay (TimeInDay)
import qualified Db.Worked as Worked (Worked(..), day, timeInDay)

-- | A 'HalfDay' could be worked or not
data HalfDay = MkHalfDayWorked Worked.Worked | MkHalfDayIdle Idle.Idle
    deriving (Eq, Generic, Show)

instance ToJSON HalfDay
instance FromJSON HalfDay

instance Display HalfDay where
    display (MkHalfDayWorked worked) = display worked
    display (MkHalfDayIdle idle) = display idle

displayHdWithDate :: HalfDay -> Utf8Builder
displayHdWithDate hd = display (hd ^. day) <> "\n" <> display hd


-- | Dispatch the day lens of the sum type
-- code inspired by: https://stackoverflow.com/questions/52832649/how-can-i-write-a-lens-for-a-sum-type
day :: Lens' HalfDay Time.Day
day f = \case
    MkHalfDayWorked worked -> MkHalfDayWorked <$> Worked.day f worked
    MkHalfDayIdle idle -> MkHalfDayIdle <$> Idle.day f idle

timeInDay :: Lens' HalfDay TimeInDay
timeInDay f = \case
    MkHalfDayWorked worked -> MkHalfDayWorked <$> Worked.timeInDay f worked
    MkHalfDayIdle idle -> MkHalfDayIdle <$> Idle.timeInDay f idle

