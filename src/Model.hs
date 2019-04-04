{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model 
where

import           RIO
import qualified RIO.Text() 
import qualified RIO.Text as Text (all, Text)
import qualified RIO.Time as Time (Day, TimeOfDay)
import qualified RIO.Char as C (isAlphaNum)

import           Data.Maybe (Maybe(..))

import           Generic.Random (genericArbitraryU)
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances.Text()

import           Database.Persist.TH 
   ( mkMigrate
   , mkPersist
   , persistLowerCase
   , share
   , sqlSettings
   )

import           HalfDayType (HalfDayType)
import           TimeInDay (TimeInDay)
import           Office (Office)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Project
    -- Fields
    name       Text
    -- Constraint
    UniqueName name 
    deriving Show
    deriving Ord
    deriving Eq
    deriving Generic
HalfDay
    -- Fields
    day             Time.Day        
    timeInDay       TimeInDay   -- morning/afternoon
    type            HalfDayType -- worked/holiday
    -- Constraint
    DayAndTimeInDay day timeInDay   -- One morning, one afternoon everyday
    deriving Show
HalfDayWorked -- Only for WorkedOpenDay
    -- Fields
    notes     Text -- default empty string
    arrived   Time.TimeOfDay 
    left      Time.TimeOfDay --Constraint Left > Arrived
    office    Office
    -- Foreign keys
    projectId ProjectId 
    halfDayId HalfDayId
    -- Constraints
    UniqueHalfDayId halfDayId
    deriving Show
|]

instance Arbitrary Project where
    arbitrary = genericArbitraryU

mkProject :: Text.Text -> Maybe Project
mkProject text = if Text.all C.isAlphaNum text 
    then Just $ Project text
    else Nothing
