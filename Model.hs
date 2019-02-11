{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import           Data.Time.Calendar
import           Data.Time.LocalTime

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import           HalfDayType
import           TimeInDay
import           Office
import           CommandLine

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Project
   -- Fields
   name       String
   -- Constraint
   UniqueName name 
   deriving Show
HalfDay
   -- Fields
   day             Day        
   timeInDay       TimeInDay   -- morning/afternoon
   type            HalfDayType -- worked/holiday
   -- Constraint
   DayAndTimeInDay day timeInDay   -- One morning, one afternoon everyday
   deriving Show
HalfDayWorked -- Only for WorkedOpenDay
   -- Fields
	notes     String -- default empty string
	arrived   TimeOfDay 
	left      TimeOfDay --Constraint Left > Arrived
   office    Office
   -- Foreign keys
   projectId ProjectId 
   halfDayId HalfDayId
   -- Constraints
   UniqueHalfDayId halfDayId
   deriving Show
|]

