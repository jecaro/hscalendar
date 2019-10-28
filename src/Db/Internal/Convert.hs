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

-- | Convert from business to db
projectToDb :: Project -> DBProject
projectToDb project = DBProject $ unProject project

-- | Convert from the db to business. May fail.
dbToProject :: DBProject -> Maybe Project
dbToProject project = mkProject $ dBProjectName project

-- | Convert from db to business. May fail.
dbToIdle :: DBHalfDay -> Maybe Idle
dbToIdle (DBHalfDay day timeInDay halfDayType) =
    mkIdle <$> dbToIdleDayType halfDayType
  where mkIdle halfDayType' = MkIdle
          { _idleDay       = day
          , _idleTimeInDay = timeInDay
          , _idleDayType   = halfDayType' }

-- | Convert an 'IdleDayType' to a 'DBHalfDayType'
idleDayTypeToDb :: IdleDayType -> DBHalfDayType
idleDayTypeToDb PaidLeave     = DBPaidLeave
idleDayTypeToDb FamilyEvent   = DBFamilyEvent
idleDayTypeToDb RTTE          = DBRTTE
idleDayTypeToDb RTTS          = DBRTTS
idleDayTypeToDb UnpaidLeave   = DBUnpaidLeave
idleDayTypeToDb PublicHoliday = DBPublicHoliday
idleDayTypeToDb PartTime      = DBPartTime

-- | Convert a 'DBHalfDayType' to an 'IdleDayType'. May fail.
dbToIdleDayType :: DBHalfDayType -> Maybe IdleDayType
dbToIdleDayType DBPaidLeave     = Just PaidLeave
dbToIdleDayType DBFamilyEvent   = Just FamilyEvent
dbToIdleDayType DBRTTE          = Just RTTE
dbToIdleDayType DBRTTS          = Just RTTS
dbToIdleDayType DBUnpaidLeave   = Just UnpaidLeave
dbToIdleDayType DBPublicHoliday = Just PublicHoliday
dbToIdleDayType DBPartTime      = Just PartTime
dbToIdleDayType DBWorked        = Nothing

-- | Create a 'Worked' with the information in the db. May fail.
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
