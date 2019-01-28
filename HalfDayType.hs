-- @HalfDayType.hs
{-# LANGUAGE TemplateHaskell #-}
module HalfDayType where

import Database.Persist.TH

data HalfDayType = Worked | Holiday
    deriving (Show, Read, Eq)
derivePersistField "HalfDayType"