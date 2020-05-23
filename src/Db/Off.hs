{-# LANGUAGE TemplateHaskell #-}

-- | Functions related to the type 'Off'
module Db.Off where

import Data.Aeson (FromJSON, ToJSON)
import Db.OffDayType
import Db.TimeInDay (TimeInDay)
import Lens.Micro.Platform (makeFields)
import RIO
import qualified RIO.Time as Time (Day)
import RIO.Time.Extended ()

-- | Simple type for a non-working half-day
data Off = MkOff
    { -- | The day
      _offDay :: !Time.Day,
      -- | morning/afternoon
      _offTimeInDay :: !TimeInDay,
      -- | The type of the non working half-day
      _offDayType :: !OffDayType
    }
    deriving (Eq, Generic, Show)

makeFields ''Off

instance ToJSON Off

instance FromJSON Off

instance Display Off where
    display off =
        "\t\t" <> display (off ^. dayType)
