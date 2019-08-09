-- | This is the internal Model. It defines the persistent data types with
-- template haskell.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Internal.DBModel
where

import           RIO
import qualified RIO.Text ()
import qualified RIO.Time as Time (Day, TimeOfDay)

import           Database.Persist.TH
   ( mkMigrate
   , mkPersist
   , persistLowerCase
   , share
   , sqlSettings
   )

import           Internal.HalfDayType (HalfDayType(..))
import           Office (Office)
import           TimeInDay (TimeInDay)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DBProject
    -- Fields
    name       Text
    -- Constraint
    UniqueName name 
    deriving Show
    deriving Ord
    deriving Eq
    deriving Generic
DBHalfDay
    -- Fields
    day             Time.Day        
    timeInDay       TimeInDay   -- morning/afternoon
    type            HalfDayType -- worked/holiday
    -- Constraint
    DayAndTimeInDay day timeInDay   -- One morning, one afternoon everyday
    deriving Show
    deriving Eq
DBHalfDayWorked -- Only for WorkedOpenDay
    -- Fields
    notes     Text -- default empty string
    arrived   Time.TimeOfDay 
    left      Time.TimeOfDay --Constraint Left > Arrived
    office    Office
    -- Foreign keys
    projectId DBProjectId 
    halfDayId DBHalfDayId
    -- Constraints
    UniqueHalfDayId halfDayId
    deriving Show
    deriving Eq
|]

