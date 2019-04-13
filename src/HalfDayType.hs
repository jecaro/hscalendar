{-# LANGUAGE TemplateHaskell #-}
module HalfDayType where

import           RIO

import           Database.Persist.TH (derivePersistField)

-- | Simple sum type for setting the kind of halfday
data HalfDayType = Worked  -- ^ Working day
                 | Holiday -- ^ Holiday
    deriving (Show, Read, Eq)
derivePersistField "HalfDayType"