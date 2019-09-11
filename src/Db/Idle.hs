{-# LANGUAGE TemplateHaskell #-}
module Db.Idle 
where

import           RIO

import qualified RIO.Time as Time (Day)

import           Lens.Micro.Platform (makeFields)

import           Db.IdleDayType
import           Db.TimeInDay (TimeInDay)

data Idle = MkIdle
    { _idleDay       :: !Time.Day
    , _idleTimeInDay :: !TimeInDay
    , _idleDayType   :: !IdleDayType
    }
    deriving (Show, Eq)
makeFields ''Idle

