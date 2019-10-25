-- | Functions related to the type 'HalfDay'
module Db.HalfDay (HalfDay(..))
where

import           RIO

import           Data.Aeson (FromJSON, ToJSON)

import           Db.Idle (Idle(..))
import           Db.Worked (Worked)

-- | A 'HalfDay' could be worked or not
data HalfDay = MkHalfDayWorked Worked | MkHalfDayIdle Idle
    deriving (Eq, Generic, Show)

instance ToJSON HalfDay
instance FromJSON HalfDay
