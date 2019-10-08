{-# LANGUAGE TemplateHaskell #-}
module Db.Idle 
where

import           RIO

import qualified RIO.Time as Time (Day)

import           Data.Aeson (FromJSON, ToJSON)
import           Lens.Micro.Platform (makeFields)

import           Db.IdleDayType
import           Db.TimeInDay (TimeInDay)

data Idle = MkIdle
    { _idleDay       :: !Time.Day
    , _idleTimeInDay :: !TimeInDay
    , _idleDayType   :: !IdleDayType
    }
    deriving (Eq, Generic, Show)
makeFields ''Idle

instance ToJSON Idle
instance FromJSON Idle
