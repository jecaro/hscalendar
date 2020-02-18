-- | Contains the 'FullDay' data type
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Db.FullDay
    ( FullDay(..)
    , empty
    , morning
    , afternoon
    )
where

import           RIO

import           Data.Aeson (FromJSON, ToJSON)
import           Lens.Micro.Platform (makeFields)

import           Db.HalfDay ( HalfDay )

-- | A 'FullDay' contains a 'HalfDay' for the morning and a 'HalfDay' for the
-- afternoon
data FullDay = MkFullDay
    { _fullDayMorning :: !(Maybe HalfDay)
    , _fullDayAfternoon :: !(Maybe HalfDay)
    }
    deriving (Eq, Generic, Show)
makeFields ''FullDay

instance FromJSON FullDay
instance ToJSON FullDay

instance Display (Maybe HalfDay) where
    display (Just hd) = display hd
    display Nothing = "Nothing"

instance Display FullDay where
    display fullDay =
           display (fullDay ^. morning) <> "\n"
        <> display (fullDay ^. afternoon)

-- | Create an empty 'FullDay'
empty :: FullDay
empty = MkFullDay Nothing Nothing
