{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Db.FullDay
    ( FullDay(..)
    , emptyFullDay
    , morning
    , afternoon
    )
where

import           RIO

import           Lens.Micro.Platform (makeFields)

import           Db.HalfDay ( HalfDay )

data FullDay = MkFullDay
    { _fullDayMorning :: !(Maybe HalfDay)
    , _fullDayAfternoon :: !(Maybe HalfDay)
    }
makeFields ''FullDay

instance Display (Maybe HalfDay) where
    display (Just hd) = display hd
    display Nothing = "Nothing"

instance Display FullDay where
    display fullDay =
           display (fullDay ^. morning) <> "\n"
        <> display (fullDay ^. afternoon)

emptyFullDay :: FullDay
emptyFullDay = MkFullDay Nothing Nothing
