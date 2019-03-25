{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import           RIO
import qualified RIO.Text() 
import qualified RIO.Time as Time (Day, TimeOfDay)

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

