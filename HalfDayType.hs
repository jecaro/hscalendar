-- @HalfDayType.hs
{-# LANGUAGE TemplateHaskell #-}
module HalfDayType where

import           RIO

import           Database.Persist.TH (derivePersistField)

data HalfDayType = Worked | Holiday
    deriving (Show, Read, Eq)
derivePersistField "HalfDayType"