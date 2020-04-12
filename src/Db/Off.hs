-- | Functions related to the type 'Off'
{-# LANGUAGE TemplateHaskell #-}
module Db.Off
where

import           RIO

import qualified RIO.Time as Time (Day)
import           RIO.Time.Extended ()

import           Data.Aeson (FromJSON, ToJSON)
import           Lens.Micro.Platform (makeFields)

import           Db.OffDayType
import           Db.TimeInDay (TimeInDay)

-- | Simple type for a non-working half-day
data Off = MkOff
    { _offDay       :: !Time.Day    -- ^ The day
    , _offTimeInDay :: !TimeInDay   -- ^ morning/afternoon
    , _offDayType   :: !OffDayType -- ^ The type of the non working half-day
    }
    deriving (Eq, Generic, Show)
makeFields ''Off

instance ToJSON Off
instance FromJSON Off

instance Display Off where
    display off
        =  "\t\t" <> display (off ^. dayType)
