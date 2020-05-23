-- | The functions to convert the data between the DBModel and the business model
module Db.Internal.Convert
    ( dbToOffDayType,
      dbToOff,
      dbToProject,
      dbToWorked,
      offDayTypeToDb,
      projectToDb,
    )
where

import Db.Internal.DBHalfDayType (DBHalfDayType (..))
import Db.Internal.DBModel
import Db.Notes (mkNotes)
import Db.Off (Off (..))
import Db.OffDayType (OffDayType (..))
import Db.Project (Project, mkProject, unProject)
import Db.Worked (Worked (..))
import RIO

-- Conversion functions

-- | Convert from business to db
projectToDb :: Project -> DBProject
projectToDb project = DBProject $ unProject project

-- | Convert from the db to business. May fail.
dbToProject :: DBProject -> Maybe Project
dbToProject project = mkProject $ dBProjectName project

-- | Convert from db to business. May fail.
dbToOff :: DBHalfDay -> Maybe Off
dbToOff (DBHalfDay day timeInDay halfDayType) =
    mkOff <$> dbToOffDayType halfDayType
    where
        mkOff halfDayType' =
            MkOff
                { _offDay = day,
                  _offTimeInDay = timeInDay,
                  _offDayType = halfDayType'
                }

-- | Convert an 'OffDayType' to a 'DBHalfDayType'
offDayTypeToDb :: OffDayType -> DBHalfDayType
offDayTypeToDb PaidLeave = DBPaidLeave
offDayTypeToDb FamilyEvent = DBFamilyEvent
offDayTypeToDb RTTE = DBRTTE
offDayTypeToDb RTTS = DBRTTS
offDayTypeToDb UnpaidLeave = DBUnpaidLeave
offDayTypeToDb PublicHoliday = DBPublicHoliday
offDayTypeToDb PartTime = DBPartTime

-- | Convert a 'DBHalfDayType' to an 'OffDayType'. May fail.
dbToOffDayType :: DBHalfDayType -> Maybe OffDayType
dbToOffDayType DBPaidLeave = Just PaidLeave
dbToOffDayType DBFamilyEvent = Just FamilyEvent
dbToOffDayType DBRTTE = Just RTTE
dbToOffDayType DBRTTS = Just RTTS
dbToOffDayType DBUnpaidLeave = Just UnpaidLeave
dbToOffDayType DBPublicHoliday = Just PublicHoliday
dbToOffDayType DBPartTime = Just PartTime
dbToOffDayType DBWorked = Nothing

-- | Create a 'Worked' with the information in the db. May fail.
dbToWorked :: DBHalfDay -> DBHalfDayWorked -> DBProject -> Maybe Worked
dbToWorked
    (DBHalfDay day timeInDay DBWorked)
    (DBHalfDayWorked dbNotes arrived left office _ _)
    dbProject = do
        notes <- mkNotes dbNotes
        project <- dbToProject dbProject
        pure
            MkWorked
                { _workedDay = day,
                  _workedTimeInDay = timeInDay,
                  _workedArrived = arrived,
                  _workedLeft = left,
                  _workedOffice = office,
                  _workedNotes = notes,
                  _workedProject = project
                }
dbToWorked _ _ _ = Nothing
