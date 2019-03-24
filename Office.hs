-- @Office.hs
{-# LANGUAGE TemplateHaskell #-}
module Office where

import           RIO

import           Database.Persist.TH (derivePersistField)

data Office = Rennes | Home | Poool
    deriving (Show, Read, Eq)
derivePersistField "Office"