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
import qualified RIO.Text ()

import           Internal.HalfDayType (HalfDayType(..))
import           Internal.Model 
import qualified IdleDayType as IDT
import           Idle(Idle(..))
import           Notes(Notes(..))
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

dbToWorked :: DBHalfDay -> DBHalfDayWorked -> DBProject -> Maybe Worked
dbToWorked (DBHalfDay day timeInDay Worked)
    (DBHalfDayWorked notes arrived left office _ _) project = Just $ MkWorked
        { _workedDay       = day
        , _workedTimeInDay = timeInDay
        , _workedArrived   = arrived
        , _workedLeft      = left
        , _workedOffice    = office
        , _workedNotes     = MkNotes notes
        , _workedProject   = dbToProject project
        }
dbToWorked _ _ _ = Nothing
