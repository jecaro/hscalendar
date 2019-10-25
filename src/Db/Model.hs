-- | The main API
module Db.Model
    ( 
    -- * Exceptions
      ProjExists(..)
    , ProjHasHd(..)
    , ProjNotFound(..)
    , HdNotFound(..)
    , TimesAreWrong(..)
    -- * Half-day functions
    , hdGet
    , hdRm
    , hdSetHoliday
    , hdSetWork
    , hdSetArrived
    , hdSetArrivedAndLeft
    , hdSetLeft
    , hdSetNotes
    , hdSetOffice
    , hdSetProject
    -- * Project functions
    , projAdd
    , projExists
    , projList
    , projRename
    , projRm
    -- * Misc
    , cleanDB
    , migrateAll
    , showDay
    , showTime
    )

where 

import           RIO hiding (on, (^.)) -- We use esqueleto symbols
import           RIO.List as L (headMaybe)
import qualified RIO.Text as Text (intercalate, pack, unpack)
import qualified RIO.Time as Time 
    ( Day
    , TimeOfDay(..)
    , defaultTimeLocale 
    , formatTime
    , toGregorian
    )

import           Control.Monad (void, when)
import           Control.Monad.IO.Class (MonadIO)
import           Database.Esqueleto 
    ( LeftOuterJoin(..)
    , from
    , just
    , on
    , select
    , val
    , where_
    , (?.)
    , (&&.)
    , (^.)
    , (==.)
    )
import           Database.Persist 
    ( Entity(..)
    , Filter
    , SelectOpt(Asc)
    , Key
    , delete
    , deleteWhere
    , getBy
    , insert
    , replace
    , selectList
    , selectFirst
    , update
    , (=.)
    )
import qualified Database.Persist as P ((==.))
import           Database.Persist.Sql (SqlPersistT, toSqlKey) 

import           Data.Maybe (isJust)
import           Formatting.Extended (formatTwoDigitsPadZero)

import           Db.HalfDay (HalfDay(..))
import           Db.IdleDayType (IdleDayType(..))
import           Db.Project (Project, unProject)
import           Db.Notes (Notes, unNotes)
import           Db.Office (Office(..))
import           Db.TimeInDay (TimeInDay(..), other)

import           Db.Internal.DBHalfDayType (DBHalfDayType(..))
import           Db.Internal.DBModel
import           Db.Internal.Convert 
    ( dbToIdle
    , dbToProject
    , dbToWorked
    , idleDayTypeToDb
    , projectToDb
    )

-- Exceptions that are likely to occure

-- | The requested project has not been found
newtype ProjNotFound = ProjNotFound Project

instance Exception ProjNotFound

instance Show ProjNotFound where
    show (ProjNotFound project) = "The project " <> name <> " is not in the database"
      where name = Text.unpack (unProject project)

-- | A project with the same name already exists in the db
newtype ProjExists = ProjExists Project

instance Exception ProjExists

instance Show ProjExists where
    show (ProjExists project) = "The project " <> name <> " exists in the database"
      where name = Text.unpack (unProject project)

-- | The project has associated hds
newtype ProjHasHd = ProjHasHd Project

instance Exception ProjHasHd

instance Show ProjHasHd where
    show (ProjHasHd project) = "The project " <> name <> " has associated half-day work"
      where name = Text.unpack (unProject project)

-- | There is no record for specified half-day
data HdNotFound = HdNotFound Time.Day TimeInDay

instance Exception HdNotFound

instance Show HdNotFound where
    show (HdNotFound day tid) = "Nothing for " <> Text.unpack (showDay day) <> " " <> show tid

-- | Given times are wrong
data TimesAreWrong = TimesAreWrong

instance Exception TimesAreWrong 

instance Show TimesAreWrong where
    show _ = "Times are wrong"

-- The following exceptions should never happen

-- | Inconsistency in the DB
data DbInconsistency = DbInconsistency

instance Exception DbInconsistency

instance Show DbInconsistency where
    show _ = "Warning db inconsistency"

-- Misc 

-- | Convert a 'Time.Day' to a string in the form dd-mm-yyyy weekday
showDay :: Time.Day -> Text
showDay day =  Text.intercalate "-" (fmap formatTwoDigitsPadZero [d, m, intY]) 
            <> " " 
            <> Text.pack weekDay
  where (y, m, d) = Time.toGregorian day
        intY = fromIntegral y
        weekDay = Time.formatTime Time.defaultTimeLocale "%a" day

-- | Print time in a friendly format ex 9:00
showTime :: Time.TimeOfDay -> Text
showTime (Time.TimeOfDay h m _) = 
            Text.intercalate ":" $ fmap formatTwoDigitsPadZero [h, m]

-- | Clean up the db
cleanDB :: (MonadIO m) => SqlPersistT m ()
cleanDB = do
    deleteWhere ([] :: [Filter DBHalfDayWorked])
    deleteWhere ([] :: [Filter DBHalfDay])
    deleteWhere ([] :: [Filter DBProject])

-- Exported project functions 

-- | Check if a project exists 
projExists :: MonadIO m => Project -> SqlPersistT m Bool
projExists project = isJust <$> getBy (UniqueName $ unProject project)


-- | Add a project 
projAdd :: (MonadIO m) => Project -> SqlPersistT m ()
projAdd project = do
    guardProjNotExistsInt project
    void $ insert $ projectToDb project

-- | Get the list of the projects present in the database
projList :: MonadIO m => SqlPersistT m [Project]
projList = mapMaybe (dbToProject . entityVal) <$> selectList [] [Asc DBProjectName] 

-- | Delete a project 
projRm :: (MonadIO m) => Project -> SqlPersistT m ()
projRm project = do
    -- The following can throw exception same exception apply to this function
    -- so we dont catch it here
    pId <- projGetInt project 
    -- Test if there is hdw using this project
    selectFirst [DBHalfDayWorkedProjectId P.==. pId] [] >>=
        \case
            Nothing -> delete pId
            Just _  -> throwIO $ ProjHasHd project

-- | Rename a project 
projRename :: (MonadIO m) => Project -> Project -> SqlPersistT m ()
projRename p1 p2 = do
    pId <- projGetInt p1
    guardProjNotExistsInt p2
    replace pId $ projectToDb p2

-- Internal project functions

-- | Get a project with error handling
projGetInt :: (MonadIO m) => Project -> SqlPersistT m (Key DBProject)
projGetInt project = getBy (UniqueName $ unProject project) >>= 
    \case
        Nothing             -> throwIO $ ProjNotFound project
        Just (Entity pId _) -> return pId

-- | Guard to check if a project is already present in the db. If so, raise an
-- exception
guardProjNotExistsInt :: (MonadIO m) => Project -> SqlPersistT m ()
guardProjNotExistsInt project = do
    exists <- projExists project
    when exists (throwIO $ ProjExists project)

-- Exported hd functions

-- | This is the main request function
hdGet
    :: (MonadIO m, MonadUnliftIO m) 
    => Time.Day
    -> TimeInDay 
    -> SqlPersistT m HalfDay
hdGet day tid = 
    (select $ from $ \(hd `LeftOuterJoin` mbHdw `LeftOuterJoin` mbProj) -> do
        where_ (hd ^. DBHalfDayDay        ==. val day &&.
                hd ^. DBHalfDayTimeInDay  ==. val tid)           
        on (mbProj ?. DBProjectId    ==. mbHdw ?. DBHalfDayWorkedProjectId)
        on (just (hd ^. DBHalfDayId) ==. mbHdw ?. DBHalfDayWorkedHalfDayId)
        return (hd, mbHdw, mbProj)) >>=
    \case
        []    -> throwIO $ HdNotFound day tid
        (x:_) -> case x of
            (Entity _ hd, Nothing, Nothing) -> 
                case dbToIdle hd of
                    Nothing -> throwIO DbInconsistency
                    Just idle -> return $ MkHalfDayIdle idle
            (Entity _ hd, Just (Entity _ hdw), Just (Entity _ proj)) -> 
                case dbToWorked hd hdw proj of
                    Nothing -> throwIO DbInconsistency
                    Just worked -> return $ MkHalfDayWorked worked
            _ -> throwIO $ HdNotFound day tid

-- | Set the office for a day-time in day
hdSetOffice :: (MonadIO m) => Time.Day -> TimeInDay -> Office -> SqlPersistT m ()
hdSetOffice day tid office = do
    (_, Entity hdwId _, _) <- hdHdwProjGetInt day tid
    update hdwId [DBHalfDayWorkedOffice =. office]

-- | Set the notes for a day-time in day
hdSetNotes :: (MonadIO m) => Time.Day -> TimeInDay -> Notes -> SqlPersistT m ()
hdSetNotes day tid notes = do
    (_, Entity hdwId _, _) <- hdHdwProjGetInt day tid
    update hdwId [DBHalfDayWorkedNotes =. unNotes notes]

-- | Set a work half-day with a project
hdSetProject :: (MonadIO m) => Time.Day -> TimeInDay -> Project -> SqlPersistT m () 
hdSetProject day tid project = do
    (_, Entity hdwId _, _) <- hdHdwProjGetInt day tid
    pId <- projGetInt project
    update hdwId [DBHalfDayWorkedProjectId =. pId]

-- | Set arrived time for a working half-day
hdSetArrived 
    :: (MonadIO m, MonadUnliftIO m) 
    => Time.Day 
    -> TimeInDay 
    -> Time.TimeOfDay 
    -> SqlPersistT m () 
hdSetArrived day tid tod = do
    (_, Entity hdwId hdw, _) <- hdHdwProjGetInt day tid
    let hdw' = hdw { dBHalfDayWorkedArrived = tod }
    guardNewTimesAreOk day tid hdw' 
    replace hdwId hdw'

-- | Set left time for a working half-day
hdSetLeft 
    :: (MonadIO m, MonadUnliftIO m) 
    => Time.Day 
    -> TimeInDay 
    -> Time.TimeOfDay 
    -> SqlPersistT m () 
hdSetLeft day tid tod = do
    (_, Entity hdwId hdw, _) <- hdHdwProjGetInt day tid
    let hdw' = hdw { dBHalfDayWorkedLeft = tod }
    guardNewTimesAreOk day tid hdw' 
    replace hdwId hdw'

-- | Set both arrived and left times for a working half-day
hdSetArrivedAndLeft 
    :: (MonadIO m, MonadUnliftIO m) 
    => Time.Day 
    -> TimeInDay 
    -> Time.TimeOfDay 
    -> Time.TimeOfDay 
    -> SqlPersistT m () 
hdSetArrivedAndLeft day tid tArrived tLeft = do
    (_, Entity hdwId hdw, _) <- hdHdwProjGetInt day tid
    let hdw' = hdw { dBHalfDayWorkedArrived = tArrived
                   , dBHalfDayWorkedLeft    = tLeft }
    guardNewTimesAreOk day tid hdw' 
    replace hdwId hdw'

-- | Set a half-day as holiday
hdSetHoliday 
    :: (MonadIO m, MonadUnliftIO m) 
    => Time.Day 
    -> TimeInDay 
    -> IdleDayType
    -> SqlPersistT m () 
hdSetHoliday day tid hdt = try (hdGetInt day tid) >>=
    \case 
        -- Create a new entry
        Left (HdNotFound _ _) -> void $ insert $ DBHalfDay day tid dbHdt
        -- Edit existing entry
        Right (Entity hdId _)   -> do
            -- Delete entry from HalfDayWorked if it exists
            deleteWhere [DBHalfDayWorkedHalfDayId P.==. hdId]
            -- Update entry
            update hdId [DBHalfDayType =. dbHdt]
  where dbHdt = idleDayTypeToDb hdt

-- | Set a half-day as working on a project. 
hdSetWork 
    :: (MonadIO m, MonadUnliftIO m) 
    => Time.Day 
    -> TimeInDay 
    -> Project 
    -> Office
    -> Time.TimeOfDay
    -> Time.TimeOfDay
    -> SqlPersistT m () 
hdSetWork day tid project office tArrived tLeft = do 
    -- Get the proj entry if it exists
    projId <- projGetInt project
    let hdw = DBHalfDayWorked "" tArrived tLeft office (toSqlKey 0) (toSqlKey 0)
    guardNewTimesAreOk day tid hdw
    eiHd <- try $ hdGetInt day tid
    hdId <- case eiHd of
        -- Create a new entry
        Left (HdNotFound _ _) -> insert $ DBHalfDay day tid DBWorked 
        -- Edit existing entry
        Right (Entity hdId _) -> do
          -- Update entry
          update hdId [DBHalfDayType =. DBWorked]
          return hdId
    let hdw' = hdw { dBHalfDayWorkedProjectId = projId
                   , dBHalfDayWorkedHalfDayId = hdId }
    getBy (UniqueHalfDayId hdId) >>= 
        \case
            Nothing               -> void $ insert hdw'
            Just (Entity hdwId _) -> replace hdwId hdw'

-- | Remove a half-day from the db
hdRm :: (MonadIO m) => Time.Day -> TimeInDay -> SqlPersistT m () 
hdRm day tid = do
    (Entity hdId _) <- hdGetInt day tid
    -- Delete entry from HalfDayWorked if it exists
    deleteWhere [DBHalfDayWorkedHalfDayId P.==. hdId]
    delete hdId

-- Internal project functions

-- | Get a half day if it exists or raise an exception
hdGetInt :: (MonadIO m) => Time.Day -> TimeInDay -> SqlPersistT m (Entity DBHalfDay)
hdGetInt day tid = getBy (DayAndTimeInDay day tid) >>=
    \case 
        Nothing -> throwIO $ HdNotFound day tid
        Just e  -> return e

-- | Private function to get a half-day work along with the project from a day
-- and a time in day
hdHdwProjGetInt 
    :: (MonadIO m) 
    => Time.Day 
    -> TimeInDay 
    -> SqlPersistT m (Entity DBHalfDay, Entity DBHalfDayWorked, Entity DBProject)
hdHdwProjGetInt day tid = do
    hdHdwProjs <- select $ from $ \(hd, hdw, proj) -> do
        where_ (hd  ^. DBHalfDayDay             ==. val day             &&.
                hd  ^. DBHalfDayTimeInDay       ==. val tid             &&.
                hdw ^. DBHalfDayWorkedProjectId ==. proj ^. DBProjectId &&. 
                hdw ^. DBHalfDayWorkedHalfDayId ==. hd ^. DBHalfDayId)
        return (hd, hdw, proj)
    maybe (throwIO $ HdNotFound day tid) return (L.headMaybe hdHdwProjs)

-- hd private functions

-- | Check that the constraints on the times are valid between the two half-days
timesAreOrderedInDay 
    :: TimeInDay 
    -> DBHalfDayWorked 
    -> Maybe DBHalfDayWorked 
    -> Bool
timesAreOrderedInDay Morning hdw mbOtherHdw = 
    isOrdered $ timesOfDay hdw ++ otherTimes
  where otherTimes = concatMap timesOfDay mbOtherHdw
-- We switch the arguments and call the same function
timesAreOrderedInDay Afternoon hdw (Just otherHdw) =
    timesAreOrderedInDay Morning otherHdw (Just hdw)
-- Afternoon only, just need to check for half day
timesAreOrderedInDay Afternoon hdw Nothing = isOrdered $ timesOfDay hdw

-- | Make sure that the times in hdw are ok:
--   - arrived < left
--   - consistent with the potential other hdw
guardNewTimesAreOk :: (MonadIO m, MonadUnliftIO m)
    => Time.Day
    -> TimeInDay
    -> DBHalfDayWorked
    -> SqlPersistT m ()
guardNewTimesAreOk day tid hdw = do
    eiHdHdwProj <- try $ hdHdwProjGetInt day $ other tid
    let mbOtherHdw = case eiHdHdwProj of
          Left (HdNotFound _ _)      -> Nothing
          Right (_, Entity _ oHdw, _) -> Just oHdw
    -- Check if it works
    unless (timesAreOrderedInDay tid hdw mbOtherHdw) (throwIO TimesAreWrong)

-- | Return the times in the day in a list
timesOfDay :: DBHalfDayWorked -> [Time.TimeOfDay]
timesOfDay hdw = [dBHalfDayWorkedArrived hdw, dBHalfDayWorkedLeft hdw]

-- | Return true if the list is sorted
isOrdered :: (Ord a) => [a] -> Bool
isOrdered []       = True
isOrdered [_]      = True
isOrdered (x:y:xs) = x <= y && isOrdered (y:xs)

