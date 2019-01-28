-- @Office.hs
{-# LANGUAGE TemplateHaskell #-}
module Office where

import Database.Persist.TH

data Office = Rennes | Home
    deriving (Show, Read, Eq)
derivePersistField "Office"