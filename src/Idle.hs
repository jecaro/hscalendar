{-# LANGUAGE TemplateHaskell #-}
module Idle 
where

import           RIO

import qualified RIO.Time as Time (Day)

import           Lens.Micro.Platform (makeFields)

import           IdleDayType
import           TimeInDay (TimeInDay)

data Idle = MkIdle
    { _idleDay       :: !Time.Day
    , _idleTimeInDay :: !TimeInDay
    , _idleDayType   :: !IdleDayType
    }
    deriving (Show, Eq)
makeFields ''Idle

