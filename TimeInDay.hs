-- @TimeInDay.hs
{-# LANGUAGE TemplateHaskell #-}
module TimeInDay where

import           RIO

import           Database.Persist.TH (derivePersistField)

data TimeInDay = Morning | Afternoon
    deriving (Show, Read, Eq, Ord)
derivePersistField "TimeInDay"

other :: TimeInDay -> TimeInDay
other Morning   = Afternoon
other Afternoon = Morning