{-# LANGUAGE TemplateHaskell #-}
module Office where

import           RIO

import           Database.Persist.TH (derivePersistField)

-- | Simple sum type for defining a work location
data Office = Rennes | Home | Poool
    deriving (Show, Read, Eq)
derivePersistField "Office"