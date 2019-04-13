{-# LANGUAGE TemplateHaskell #-}
module TimeInDay where

import           RIO

import           Database.Persist.TH (derivePersistField)

-- | Simple sum type of defining the time in the day
data TimeInDay = Morning | Afternoon
    deriving (Show, Read, Eq, Ord)
derivePersistField "TimeInDay"

-- | Switch to the other TimeInDay
other :: TimeInDay -> TimeInDay
other Morning   = Afternoon
other Afternoon = Morning