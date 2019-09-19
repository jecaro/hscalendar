-- | The functions to convert the data between the DBModel and the business model
module Db.Internal.Convert
    ( dbToIdleDayType
    , dbToIdle
    , dbToProject
    , dbToWorked
    , idleDayTypeToDb
    , projectToDb
    )
where

import           RIO

import           Db.Internal.DBHalfDayType (DBHalfDayType(..))
import           Db.Internal.DBModel 
import           Db.IdleDayType (IdleDayType(..))
import           Db.Idle (Idle(..))
import           Db.Notes (mkNotes)
import           Db.Project (Project, mkProject, unProject)
import           Db.Worked (Worked(..))

-- Conversion functions

projectToDb :: Project -> DBProject
projectToDb project = DBProject $ unProject project

dbToProject :: DBProject -> Maybe Project
dbToProject project = mkProject $ dBProjectName project

dbToIdle :: DBHalfDay -> Maybe Idle
dbToIdle (DBHalfDay day timeInDay halfDayType) = 
    mkIdle <$> dbToIdleDayType halfDayType
  where mkIdle halfDayType' = MkIdle
          { _idleDay       = day
          , _idleTimeInDay = timeInDay
          , _idleDayType   = halfDayType' }

idleDayTypeToDb :: IdleDayType -> DBHalfDayType
idleDayTypeToDb PayedLeave    = DBPayedLeave
idleDayTypeToDb FamilyEvent   = DBFamilyEvent
idleDayTypeToDb RTTE          = DBRTTE
idleDayTypeToDb RTTS          = DBRTTS
idleDayTypeToDb UnpayedLeave  = DBUnpayedLeave
idleDayTypeToDb PublicHoliday = DBPublicHoliday
idleDayTypeToDb PartTime      = DBPartTime

dbToIdleDayType :: DBHalfDayType -> Maybe IdleDayType
dbToIdleDayType DBPayedLeave    = Just PayedLeave
dbToIdleDayType DBFamilyEvent   = Just FamilyEvent
dbToIdleDayType DBRTTE          = Just RTTE
dbToIdleDayType DBRTTS          = Just RTTS
dbToIdleDayType DBUnpayedLeave  = Just UnpayedLeave
dbToIdleDayType DBPublicHoliday = Just PublicHoliday
dbToIdleDayType DBPartTime      = Just PartTime
dbToIdleDayType DBWorked        = Nothing

dbToWorked :: DBHalfDay -> DBHalfDayWorked -> DBProject -> Maybe Worked
dbToWorked (DBHalfDay day timeInDay DBWorked)
    (DBHalfDayWorked dbNotes arrived left office _ _) dbProject = do
        notes <- mkNotes dbNotes
        project <- dbToProject dbProject
        return MkWorked
            { _workedDay       = day
            , _workedTimeInDay = timeInDay
            , _workedArrived   = arrived
            , _workedLeft      = left
            , _workedOffice    = office
            , _workedNotes     = notes
            , _workedProject   = project
            }
dbToWorked _ _ _ = Nothing