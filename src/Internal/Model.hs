-- | This is the internal Model. It defines the persistent data types with
-- template haskell.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Internal.Model
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

import           HalfDayType (HalfDayType(..))
import qualified IdleDayType as IDT
import qualified NewModel
import           Office (Office)
import           TimeInDay (TimeInDay)

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
    deriving Eq
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
    deriving Eq
|]

-- Conversion functions

projectToDb :: NewModel.Project -> Project
projectToDb project = Project $ NewModel.unProject project

dbToProject :: Project -> NewModel.Project
dbToProject project = NewModel.MkProject $ projectName project

dbToIdle :: HalfDay -> Maybe NewModel.Idle
dbToIdle (HalfDay day timeInDay halfDayType) = 
    mkIdle <$> dbToIdleDayType halfDayType
  where mkIdle halfDayType' = NewModel.MkIdle
          { NewModel._idleDay       = day
          , NewModel._idleTimeInDay = timeInDay
          , NewModel._idleDayType   = halfDayType' }

idleDayTypeToDb :: IDT.IdleDayType -> HalfDayType
idleDayTypeToDb IDT.PayedLeave    = PayedLeave
idleDayTypeToDb IDT.FamilyEvent   = FamilyEvent
idleDayTypeToDb IDT.RTTE          = RTTE
idleDayTypeToDb IDT.RTTS          = RTTS
idleDayTypeToDb IDT.UnpayedLeave  = UnpayedLeave
idleDayTypeToDb IDT.PublicHoliday = PublicHoliday
idleDayTypeToDb IDT.PartTime      = PartTime

dbToIdleDayType :: HalfDayType -> Maybe IDT.IdleDayType
dbToIdleDayType PayedLeave    = Just IDT.PayedLeave
dbToIdleDayType FamilyEvent   = Just IDT.FamilyEvent
dbToIdleDayType RTTE          = Just IDT.RTTE
dbToIdleDayType RTTS          = Just IDT.RTTS
dbToIdleDayType UnpayedLeave  = Just IDT.UnpayedLeave
dbToIdleDayType PublicHoliday = Just IDT.PublicHoliday
dbToIdleDayType PartTime      = Just IDT.PartTime
dbToIdleDayType Worked        = Nothing

dbToWorked :: HalfDay -> HalfDayWorked -> Project -> Maybe NewModel.Worked
dbToWorked (HalfDay day timeInDay Worked)
    (HalfDayWorked notes arrived left office _ _) project = Just $ NewModel.MkWorked
        { NewModel._workedDay       = day
        , NewModel._workedTimeInDay = timeInDay
        , NewModel._workedArrived   = arrived
        , NewModel._workedLeft      = left
        , NewModel._workedOffice    = office
        , NewModel._workedNotes     = NewModel.MkNotes notes
        , NewModel._workedProject   = dbToProject project
        }
dbToWorked _ _ _ = Nothing
