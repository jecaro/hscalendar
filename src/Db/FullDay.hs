-- | Contains the 'FullDay' data type
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Db.FullDay
    ( FullDay(..)
    , day
    , empty
    , morning
    , afternoon
    )
where

import           RIO
import qualified RIO.Time as Time (Day)

import           Data.Aeson (FromJSON, ToJSON)
import           Lens.Micro.Platform (makeFields)

import           Db.HalfDay ( HalfDay )

-- | A 'FullDay' contains a 'HalfDay' for the morning and a 'HalfDay' for the
-- afternoon
data FullDay a = MkFullDay
    { _fullDayDay :: !Time.Day
    , _fullDayMorning :: !a
    , _fullDayAfternoon :: !a
    }
    deriving (Eq, Functor, Generic, Show)
makeFields ''FullDay

instance FromJSON (FullDay (Maybe HalfDay))
instance ToJSON (FullDay (Maybe HalfDay))

instance Display (Maybe HalfDay) where
    display (Just hd) = display hd
    display Nothing = "Nothing"

instance Display (FullDay (Maybe HalfDay)) where
    display fullDay
        =  display (fullDay ^. morning) <> "\n"
        <> display (fullDay ^. afternoon)

-- | Create an empty 'FullDay'
empty :: a -> Time.Day -> FullDay a
empty a day' = MkFullDay
    { _fullDayDay = day'
    , _fullDayMorning = a
    , _fullDayAfternoon = a
    }

