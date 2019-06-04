-- | This module contains the functions defining the CRUD API to request and
-- update the database. All persistent fields are hidden from the public API.
module ModelFcts 
    (
    -- * Types
      ProjExists(..)
    , ProjNotFound(..)
    , HdNotFound(..)
    , HdwNotFound(..)
    , TimesAreWrong(..)
    -- * Half-day functions
    , hdHdwProjGet
    , hdRm
    , hdSetHoliday
    , hdSetWork
    , hdwSetArrived
    , hdwSetArrivedAndLeft
    , hdwSetLeft
    , hdwSetNotes
    , hdwSetOffice
    , hdwSetProject
    -- * Project functions
    , projAdd
    , projExists
    , projList
    , projRename
    , projRm
    -- * Misc
    , showDay
    ) where

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

import           Refined (unrefine)

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
import           Database.Persist.Sqlite 
    ( Entity(..)
    , SelectOpt(Asc)
    , SqlPersistT
    , Key
    , delete
    , deleteWhere
    , getBy
    , insert
    , replace
    , selectList
    , update
    , (=.)
    )
import qualified Database.Persist.Sqlite as P ((==.))

import           Data.Maybe (isJust)
import           Formatting (int, left, sformat, (%.))

import           HalfDayType(HalfDayType(..))
import           Model
import           Office (Office(..))
import           TimeInDay(TimeInDay(..), other)

-- Exceptions that are likely to occure

-- | The requested project has not be found
newtype ProjNotFound = ProjNotFound Project

instance Exception ProjNotFound

instance Show ProjNotFound where
    show (ProjNotFound project) = "The project " <> name <> " is not in the database"
      where name = Text.unpack (projectName project)

-- | A project with the same name allready exists in the db
newtype ProjExists = ProjExists Project

instance Exception ProjExists

instance Show ProjExists where
    show (ProjExists project) = "The project " <> name <> " exists in the database"
      where name = Text.unpack (projectName project)

-- | There is no record for specified HD
data HdNotFound = HdNotFound Time.Day TimeInDay

instance Exception HdNotFound

instance Show HdNotFound where
    show (HdNotFound day tid) = "Nothing for " <> Text.unpack (showDay day) <> " " <> show tid

-- | There is no work record in HDW for specified day and time in day
data HdwNotFound = HdwNotFound Time.Day TimeInDay

instance Exception HdwNotFound

instance Show HdwNotFound where
    show (HdwNotFound day tid) = "No half-day worked entry for " 
        <> Text.unpack (showDay day) <> " " <> show tid

-- | Given times are wrong
data TimesAreWrong = TimesAreWrong

instance Exception TimesAreWrong 

instance Show TimesAreWrong where
    show _ = "Times are wrong"

-- The following exceptions should never happen

-- | Specified project id has not been found
newtype ProjIdNotFound = ProjIdNotFound ProjectId

instance Exception ProjIdNotFound

instance Show ProjIdNotFound where
    show (ProjIdNotFound pId) = "No project entry for " <> show pId

-- | Inconsistency in the DB
data DbInconsistency = DbInconsistency

instance Exception DbInconsistency

instance Show DbInconsistency where
    show _ = "Warning db inconsistency"

-- Misc 

-- | Convert a Day to a string in the form dd-mm-yyyy
showDay :: Time.Day -> Text
showDay day =  Text.intercalate "-" (fmap printNum [d, m, intY]) 
            <> " " 
            <> Text.pack weekDay
  where (y, m, d) = Time.toGregorian day
        intY = fromIntegral y
        printNum = sformat (left 2 '0' %. int) 
        weekDay = Time.formatTime Time.defaultTimeLocale "%a" day

-- Exported project functions 

-- | Check if a project exists 
projExists :: MonadIO m => Project -> SqlPersistT m Bool
projExists project = isJust <$> getBy (UniqueName $ projectName project)

-- | Add a project 
projAdd :: (MonadIO m) => Project -> SqlPersistT m ()
projAdd project = do
    guardProjNotExistsInt project
    void $ insert project

-- | Get the list of the projects present in the database
projList :: MonadIO m => SqlPersistT m [Project]
projList = map entityVal <$> selectList [] [Asc ProjectName] 

-- | Delete a project 
projRm :: (MonadIO m) => Project -> SqlPersistT m ()
projRm project = do
    -- The following can throw exception same exception apply to this function
    -- so we dont catch it here
    pId <- projGetInt project 
    deleteWhere [HalfDayWorkedProjectId P.==. pId]
    delete pId

-- | Rename a project 
projRename :: (MonadIO m) => Project -> Project -> SqlPersistT m ()
projRename p1 p2 = do
    pId <- projGetInt p1
    guardProjNotExistsInt p2
    replace pId p2

-- Internal project functions

-- | Get a project with error handling
projGetInt :: (MonadIO m) => Project -> SqlPersistT m (Key Project)
projGetInt project = getBy (UniqueName $ projectName project) >>= 
    \case
        Nothing             -> throwIO $ ProjNotFound project
        Just (Entity pId _) -> return pId

-- | Guard to check if a project is allready present in the db. If so, raise an
-- exception
guardProjNotExistsInt :: (MonadIO m) => Project -> SqlPersistT m ()
guardProjNotExistsInt project = do
    exists <- projExists project
    when exists (throwIO $ ProjExists project)

-- Exported hd functions

-- | This is the main request function
hdHdwProjGet
    :: (MonadIO m, MonadUnliftIO m) 
    => Time.Day
    -> TimeInDay 
    -> SqlPersistT m (HalfDay, Maybe (HalfDayWorked, Project))
hdHdwProjGet day tid = 
    (select $ from $ \(hd `LeftOuterJoin` mbHdw `LeftOuterJoin` mbProj) -> do
        where_ (hd ^. HalfDayDay        ==. val day &&.
                hd ^. HalfDayTimeInDay  ==. val tid)           
        on (mbProj ?. ProjectId    ==. mbHdw ?. HalfDayWorkedProjectId)
        on (just (hd ^. HalfDayId) ==. mbHdw ?. HalfDayWorkedHalfDayId)
        return (hd, mbHdw, mbProj)) >>=
    \case
        []    -> throwIO $ HdNotFound day tid
        (x:_) -> case x of
            (Entity _ hd, Nothing, Nothing) -> return (hd, Nothing)
            (Entity _ hd, Just (Entity _ hdw), Just (Entity _ proj)) 
                -> return (hd, Just (hdw, proj))
            _ -> throwIO DbInconsistency

-- | Set the office for a day-time in day
hdwSetOffice :: (MonadIO m) => Time.Day -> TimeInDay -> Office -> SqlPersistT m ()
hdwSetOffice day tid office = do
    (_, Entity hdwId _, _) <- hdHdwProjGetInt day tid
    update hdwId [HalfDayWorkedOffice =. office]

-- | Set the notes for a day-time in day
hdwSetNotes :: (MonadIO m) => Time.Day -> TimeInDay -> NotesText -> SqlPersistT m ()
hdwSetNotes day tid notes = do
    (_, Entity hdwId _, _) <- hdHdwProjGetInt day tid
    update hdwId [HalfDayWorkedNotes =. (unrefine notes)]

-- | Set a work half-day with a project
hdwSetProject :: (MonadIO m) => Time.Day -> TimeInDay -> Project -> SqlPersistT m () 
hdwSetProject day tid project = do
    (_, Entity hdwId _, _) <- hdHdwProjGetInt day tid
    pId <- projGetInt project
    update hdwId [HalfDayWorkedProjectId =. pId]

-- | Set arrived time for a working half-day
hdwSetArrived 
    :: (MonadIO m, MonadUnliftIO m) 
    => Time.Day 
    -> TimeInDay 
    -> Time.TimeOfDay 
    -> SqlPersistT m () 
hdwSetArrived day tid tod = do
    (eHd, eHdw, _) <- hdHdwProjGetInt day tid
    editTime eHd eHdw $ setArrived tod
  where setArrived tod' hdw = hdw { halfDayWorkedArrived = tod' }

-- | Set left time for a working half-day
hdwSetLeft 
    :: (MonadIO m, MonadUnliftIO m) 
    => Time.Day 
    -> TimeInDay 
    -> Time.TimeOfDay 
    -> SqlPersistT m () 
hdwSetLeft day tid tod = do
    (eHd, eHdw, _) <- hdHdwProjGetInt day tid
    editTime eHd eHdw $ setLeft tod
  where setLeft tod' hdw = hdw { halfDayWorkedLeft = tod' }

-- | Set both arrived and left time for a working half-day. Arrived time must be
-- < to left time
hdwSetArrivedAndLeft 
    :: (MonadIO m, MonadUnliftIO m) 
    => Time.Day 
    -> TimeInDay 
    -> Time.TimeOfDay 
    -> Time.TimeOfDay 
    -> SqlPersistT m () 
hdwSetArrivedAndLeft day tid tArrived tLeft = do
    (eHd, eHdw, _) <- hdHdwProjGetInt day tid
    editTime eHd eHdw $ setArrivedAndLeft tArrived tLeft
  where setArrivedAndLeft arrived' left' hdw = 
            hdw { halfDayWorkedArrived = arrived'
                , halfDayWorkedLeft    = left' }

-- | Set a half-day as holiday
hdSetHoliday 
    :: (MonadIO m, MonadUnliftIO m) 
    => Time.Day 
    -> TimeInDay -> SqlPersistT m () 
hdSetHoliday day tid = try (hdGetInt day tid) >>=
    \case 
        -- Create a new entry
        Left (HdNotFound _ _) -> void $ insert $ HalfDay day tid Holiday
        -- Edit existing entry
        Right (Entity hdId _)   -> do          
            -- Delete entry from HalfDayWorked if it exists
            deleteWhere [HalfDayWorkedHalfDayId P.==. hdId]
            -- Update entry
            update hdId [HalfDayType =. Holiday]

-- | Set a half-day as working on a project. It uses default values for the rest
-- of the field
hdSetWork 
    :: (MonadIO m, MonadUnliftIO m) 
    => Time.Day 
    -> TimeInDay 
    -> Project 
    -> Time.TimeOfDay
    -> Time.TimeOfDay
    -> SqlPersistT m () 
hdSetWork day tid project tArrived tLeft = do 
    projId <- projGetInt project
    eiHd <- try $ hdGetInt day tid
    hdId <- case eiHd of
        -- Create a new entry
        Left (HdNotFound _ _) -> insert $ HalfDay day tid Worked 
        -- Edit existing entry
        Right (Entity hdId _)   -> do
          -- Update entry
          update hdId [HalfDayType =. Worked]
          return hdId
    void $ insert $ HalfDayWorked "" tArrived tLeft Rennes projId hdId
   
-- | Remove a half-day from the db
hdRm :: (MonadIO m) => Time.Day -> TimeInDay -> SqlPersistT m () 
hdRm day tid = do
    (Entity hdId _) <- hdGetInt day tid
    -- Delete entry from HalfDayWorked if it exists
    deleteWhere [HalfDayWorkedHalfDayId P.==. hdId]
    delete hdId

-- Internal project functions

-- | Get a half day if it exists or raise an exception
hdGetInt :: (MonadIO m) => Time.Day -> TimeInDay -> SqlPersistT m (Entity HalfDay)
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
    -> SqlPersistT m (Entity HalfDay, Entity HalfDayWorked, Entity Project)
hdHdwProjGetInt day tid = do
    hdHdwProjs <- select $ from $ \(hd, hdw, proj) -> do
        where_ (hd  ^. HalfDayDay             ==. val day           &&.
                hd  ^. HalfDayTimeInDay       ==. val tid           &&.
                hdw ^. HalfDayWorkedProjectId ==. proj ^. ProjectId &&. 
                hdw ^. HalfDayWorkedHalfDayId ==. hd ^. HalfDayId)
        return (hd, hdw, proj)
    maybe (throwIO $ HdwNotFound day tid) return (L.headMaybe hdHdwProjs)

-- hd private functions

-- | Check that the constraints on the times are valid between the two days
timesAreOrderedInDay 
    :: TimeInDay 
    -> HalfDayWorked 
    -> Maybe HalfDayWorked 
    -> Bool
timesAreOrderedInDay Morning hdw mbOtherHdw = 
    isOrdered $ timesOfDay hdw ++ otherTimes
  where otherTimes = concatMap timesOfDay mbOtherHdw
-- We switch the arguments and call the same function
timesAreOrderedInDay Afternoon hdw (Just otherHdw) =
    timesAreOrderedInDay Morning otherHdw (Just hdw)
-- Afternoon only, just need to check for half day
timesAreOrderedInDay Afternoon hdw Nothing = isOrdered $ timesOfDay hdw

-- | Set arrived/left time
editTime :: (MonadIO m, MonadUnliftIO m)
    => Entity HalfDay
    -> Entity HalfDayWorked
    -> (HalfDayWorked -> HalfDayWorked)
    -> SqlPersistT m ()
editTime (Entity _ (HalfDay day tid _)) (Entity hdwId hdw) setTime = do
    eiHdHdwProj <- try $ hdHdwProjGetInt day $ other tid
    let mbOtherHdw = case eiHdHdwProj of
          Left (HdwNotFound _ _)      -> Nothing
          Right (_, Entity _ oHdw, _) -> Just oHdw
    -- Apply time to arrived/left hdw
    let hdw' = setTime hdw
    -- Check if it works
    when (not $ timesAreOrderedInDay tid hdw' mbOtherHdw) (throwIO $ TimesAreWrong)
    replace hdwId hdw'

-- | Return the times in the day in a list
timesOfDay :: HalfDayWorked -> [Time.TimeOfDay]
timesOfDay hdw = [halfDayWorkedArrived hdw, halfDayWorkedLeft hdw]

-- | Return true if the list is sorted
isOrdered :: (Ord a) => [a] -> Bool
isOrdered []       = True
isOrdered [_]      = True
isOrdered (x:y:xs) = x <= y && isOrdered (y:xs)

