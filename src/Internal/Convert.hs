module Internal.Convert
    ( dbToIdleDayType
    , dbToIdle
    , dbToProject
    , dbToWorked
    , idleDayTypeToDb
    , projectToDb
    )
where

import           RIO

import           Internal.DBHalfDayType (DBHalfDayType(..))
import           Internal.DBModel 
import           IdleDayType (IdleDayType(..))
import           Idle (Idle(..))
import           Notes (mkNotes)
import           Project (Project(..))
import           Worked (Worked(..))

-- Conversion functions

projectToDb :: Project -> DBProject
projectToDb project = DBProject $ unProject project

dbToProject :: DBProject -> Project
dbToProject project = MkProject $ dBProjectName project

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
    (DBHalfDayWorked dbNotes arrived left office _ _) project = do
        notes <- mkNotes dbNotes
        return MkWorked
            { _workedDay       = day
            , _workedTimeInDay = timeInDay
            , _workedArrived   = arrived
            , _workedLeft      = left
            , _workedOffice    = office
            , _workedNotes     = notes
            , _workedProject   = dbToProject project
            }
dbToWorked _ _ _ = Nothing
