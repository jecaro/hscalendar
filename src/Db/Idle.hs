-- | Functions related to the type 'Idle'
{-# LANGUAGE TemplateHaskell #-}
module Db.Idle
where

import           RIO

import qualified RIO.Time as Time (Day)

import           Data.Aeson (FromJSON, ToJSON)
import           Lens.Micro.Platform (makeFields)

import           Db.IdleDayType
import           Db.TimeInDay (TimeInDay)

-- | Simple type for a non-working half-day
data Idle = MkIdle
    { _idleDay       :: !Time.Day    -- ^ The day
    , _idleTimeInDay :: !TimeInDay   -- ^ morning/afternoon
    , _idleDayType   :: !IdleDayType -- ^ The type of the non working half-day
    }
    deriving (Eq, Generic, Show)
makeFields ''Idle

instance ToJSON Idle
instance FromJSON Idle
