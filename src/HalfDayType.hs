{-# LANGUAGE TemplateHaskell #-}
module HalfDayType 
    ( HalfDayType(..)
    ) where

import           RIO

import           Database.Persist.TH (derivePersistField)

-- | Simple sum type for setting the kind of halfday
data HalfDayType = PayedLeave
                 | FamilyEvent
                 | RTTE
                 | RTTS
                 | UnpayedLeave
                 | PublicHoliday 
                 | PartTime
                 | Worked
    deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)
derivePersistField "HalfDayType"

