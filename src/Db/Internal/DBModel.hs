-- | This is the internal model. It defines the persistent data types with
-- template haskell.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Db.Internal.DBModel
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

import           Db.Internal.DBHalfDayType (DBHalfDayType(..))
import           Db.Office (Office)
import           Db.TimeInDay (TimeInDay)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DBProject
    -- Fields
    name       Text
    -- Constraint
    UniqueName name 
    -- Type classes
    deriving Show
    deriving Ord
    deriving Eq
    deriving Generic
DBHalfDay
    -- Fields
    day             Time.Day        
    timeInDay       TimeInDay     
    type            DBHalfDayType 
    -- Constraint
    DayAndTimeInDay day timeInDay
    -- Type classes
    deriving Show
    deriving Eq
DBHalfDayWorked -- Only for WorkedOpenDay
    -- Fields
    notes     Text 
    arrived   Time.TimeOfDay 
    left      Time.TimeOfDay 
    office    Office
    -- Foreign keys
    projectId DBProjectId 
    halfDayId DBHalfDayId
    -- Constraints
    UniqueHalfDayId halfDayId
    -- Type classes
    deriving Show
    deriving Eq
|]

