module Db.HalfDay
where

import           RIO

import           Data.Aeson (FromJSON, ToJSON)

import           Db.Idle (Idle(..))
import           Db.Worked (Worked)

data HalfDay = MkHalfDayWorked Worked | MkHalfDayIdle Idle
    deriving (Eq, Generic, Show)

instance ToJSON HalfDay
instance FromJSON HalfDay
