-- | This module contains the functions defining the CRUD API to request and
-- update the database. All persistent fields are hidden from the public API.
module ModelFcts 
    (
    -- * Types
      ProjExists(..)
    , ProjNotFound(..)
    , HdNotFound(..)
    , HdwIdNotFound(..)
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

import           RIO
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
import           Database.Persist.Sqlite 
    ( Entity(..)
    , SelectOpt(Asc)
    , SqlPersistT
    , Key
    , delete
    , deleteWhere
    , get
    , getBy
    , insert
    , replace
    , selectList
    , update
    , (=.)
    , (==.)
    )

import           Data.Maybe (isJust)
import           Formatting (int, left, sformat, (%.))

import           HalfDayType(HalfDayType(..))
import           Model
import           Office (Office(..))
import           TimeInDay(TimeInDay(..), other)

-- Error strings

newtype ProjNotFound = ProjNotFound Project

instance Exception ProjNotFound

instance Show ProjNotFound where
    show (ProjNotFound project) = "The project " <> name <> " is not in the database"
      where name = Text.unpack (projectName project)

newtype ProjExists = ProjExists Project

instance Exception ProjExists

instance Show ProjExists where
    show (ProjExists project) = "The project " <> name <> " exists in the database"
      where name = Text.unpack (projectName project)

data HdNotFound = HdNotFound Time.Day TimeInDay

instance Exception HdNotFound

instance Show HdNotFound where
    show (HdNotFound day tid) = "Nothing for " <> Text.unpack (showDay day) <> " " <> show tid

newtype HdwIdNotFound = HdwIdNotFound HalfDayId

instance Exception HdwIdNotFound

instance Show HdwIdNotFound where
    show (HdwIdNotFound hdwId) = "No half-day worked entry for " <> show hdwId

newtype ProjIdNotFound = ProjIdNotFound ProjectId

instance Exception ProjIdNotFound

instance Show ProjIdNotFound where
    show (ProjIdNotFound pId) = "No project entry for " <> show pId

data DbInconsistency = DbInconsistency

instance Exception DbInconsistency

instance Show DbInconsistency where
    show _ = "Warning db inconsistency"

data TimesAreWrong = TimesAreWrong

instance Exception TimesAreWrong 

instance Show TimesAreWrong where
    show _ = "Times are wrong"

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
    deleteWhere [HalfDayWorkedProjectId ==. pId]
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
hdHdwProjGet day tid = do
    (Entity hdId hd) <- hdGetInt day tid 
    eiHdwProj <- tryJust projOrHwdIdNotFound (hdwProjGetInt hdId)
    let mbHdw = case eiHdwProj of       
           Left  _ -> Nothing 
           Right (Entity _ hdw, Entity _ project) -> Just (hdw, project)
    -- Check for consistency
    case (hd, mbHdw) of
        (HalfDay _ _ Worked, Nothing) -> throwIO DbInconsistency
        (HalfDay _ _ Holiday, Just _) -> throwIO DbInconsistency
        (_, _)                        -> return (hd, mbHdw)
  where 
    projOrHwdIdNotFound e = 
        let projNotFound = fmap toException (fromException e :: Maybe ProjIdNotFound)
            hwdIdNotFound = fmap toException (fromException e :: Maybe HdwIdNotFound)
        in projNotFound <|> hwdIdNotFound

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
            deleteWhere [HalfDayWorkedHalfDayId ==. hdId]
            -- Update entry
            update hdId [HalfDayType =. Holiday]

-- | Set a half-day as working on a project. It uses default values for the rest
-- of the field
hdSetWork 
    :: (MonadIO m, MonadUnliftIO m) 
    => Time.Day 
    -> TimeInDay 
    -> Project 
    -> SqlPersistT m () 
hdSetWork day tid project = do 
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
  where 
    tArrived = if tid == Morning 
                   then Time.TimeOfDay 9 0 0 
                   else Time.TimeOfDay 13 30 0 
    tLeft = if tid == Morning 
                then Time.TimeOfDay 12 0 0
                 else Time.TimeOfDay 17 30 0 
   
-- | Remove a half-day from the db
hdRm :: (MonadIO m) => Time.Day -> TimeInDay -> SqlPersistT m () 
hdRm day tid = do
    (Entity hdId _) <- hdGetInt day tid
    -- Delete entry from HalfDayWorked if it exists
    deleteWhere [HalfDayWorkedHalfDayId ==. hdId]
    delete hdId
 
-- Internal project functions

-- | Get a half day if it exists or raise an exception
hdGetInt :: (MonadIO m) => Time.Day -> TimeInDay -> SqlPersistT m (Entity HalfDay)
hdGetInt day tid = getBy (DayAndTimeInDay day tid) >>=
    \case 
        Nothing -> throwIO $ HdNotFound day tid
        Just e  -> return e

-- | Private function to get a half-day work along with the project from a
-- half-day id
hdwProjGetInt 
    :: (MonadIO m) 
    => HalfDayId 
    -> SqlPersistT m (Entity HalfDayWorked, Entity Project)
hdwProjGetInt hdId = getBy (UniqueHalfDayId hdId) >>=
    \case
        Nothing -> throwIO $ HdwIdNotFound hdId
        Just e@(Entity _ (HalfDayWorked _ _ _ _ pId _)) -> do
            mbProj <- get pId
            project <- case mbProj of 
                Nothing -> throwIO $ ProjIdNotFound pId
                Just p  -> return (Entity pId p)
            return (e, project)

-- | Private function to get a half-day work along with the project from a day
-- and a time in day
hdHdwProjGetInt 
    :: (MonadIO m) 
    => Time.Day 
    -> TimeInDay 
    -> SqlPersistT m (Entity HalfDay, Entity HalfDayWorked, Entity Project)
hdHdwProjGetInt day tid = do
    eHd@(Entity hdId _) <- hdGetInt day tid
    (eHdw, eProj) <- hdwProjGetInt hdId
    return (eHd, eHdw, eProj)

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
          Left (HdNotFound _ _)       -> Nothing
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

