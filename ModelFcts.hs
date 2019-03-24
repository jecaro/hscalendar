module ModelFcts 
    ( ModelException(..)
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
    , projAdd
    , projExists
    , projList
    , projRename
    , projRm
    , showDay
    ) where

import           Control.Exception.Safe 
    ( Exception
    , MonadCatch
    , MonadThrow
    , throwM
    , try
    )
import           Control.Monad (void, when)
import           Control.Monad.IO.Class (MonadIO)
import           Database.Persist.Sqlite 
    ( Entity(..)
    , SelectOpt(Asc)
    , SqlPersistT
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
import           Data.Text (Text, intercalate, pack)
import           Data.Time.Calendar (Day, toGregorian)
import           Data.Time.LocalTime (TimeOfDay(..))
import           Formatting (int, left, sformat, (%.))

import           HalfDayType(HalfDayType(..))
import           Model
import           Office (Office(..))
import           TimeInDay(TimeInDay(..), other)

newtype ModelException = ModelException Text deriving (Show)

instance Exception ModelException

-- Error strings

errProjNotFound :: Project -> Text
errProjNotFound (Project name) = "The project " <> name <> " is not in the database"

errProjExists :: Project -> Text
errProjExists (Project name) = "The project " <> name <> " exists in the database"

errHdNotFound :: Day -> TimeInDay -> Text
errHdNotFound day tid = "Nothing for " <> showDay day <> " " <> (pack . show) tid

errHdwIdNotFound :: HalfDayId -> Text
errHdwIdNotFound hdwId = "No half-day worked entry for " <> (pack . show) hdwId

errProjIdNotFound :: ProjectId -> Text
errProjIdNotFound pId = "No project entry for " <> (pack . show) pId

errDbInconsistency :: Text
errDbInconsistency = "Warning db inconsistency"

errTimesAreWrong :: Text
errTimesAreWrong = "Times are wrong"

-- Misc 

showDay :: Day -> Text
showDay day = intercalate "-" $ fmap printNum [d, m, intY]
  where (y, m, d) = toGregorian day
        intY = fromIntegral y
        printNum = sformat (left 2 '0' %. int) 

-- Exported project functions 

projExists :: MonadIO m => Project -> SqlPersistT m Bool
projExists (Project name) = isJust <$> getBy (UniqueName name)

projAdd :: (MonadIO m, MonadThrow m) => Project -> SqlPersistT m ()
projAdd project = do
    guardProjNotExistsInt project
    void $ insert project

projList :: MonadIO m => SqlPersistT m [Project]
projList = map entityVal <$> selectList [] [Asc ProjectName] 

projRm :: (MonadIO m, MonadThrow m) => Project -> SqlPersistT m ()
projRm project = do
    -- The following can throw exception same exception apply to this function
    -- so we dont catch it here
    pId <- projGetInt project 
    deleteWhere [HalfDayWorkedProjectId ==. pId]
    delete pId

projRename :: (MonadIO m, MonadThrow m) => Project -> Project -> SqlPersistT m ()
projRename p1 p2 = do
    pId <- projGetInt p1
    guardProjNotExistsInt p2
    replace pId p2

-- Internal project functions

projGetInt :: (MonadIO m, MonadThrow m) => Project -> SqlPersistT m (Key Project)
projGetInt project@(Project name) = getBy (UniqueName name) >>= 
    \case
        Nothing             -> throwM $ ModelException $ errProjNotFound project
        Just (Entity pId _) -> return pId

guardProjNotExistsInt :: (MonadIO m, MonadThrow m) => Project -> SqlPersistT m ()
guardProjNotExistsInt project = do
    exists <- projExists project
    when exists (throwM $ ModelException $ errProjExists project)

-- Exported hd functions

-- Main request function
hdHdwProjGet
    :: (MonadIO m, MonadCatch m) 
    => Day
    -> TimeInDay 
    -> SqlPersistT m (HalfDay, Maybe (HalfDayWorked, Project))
hdHdwProjGet day tid = do
    (Entity hdId hd) <- hdGetInt day tid 
    eiHdwProj <- try $ hdwProjGetInt hdId
    let mbHdw = case eiHdwProj of       
          Left (ModelException _)                -> Nothing
          Right (Entity _ hdw, Entity _ project) -> Just (hdw, project)
    -- Check for consistency
    case (hd, mbHdw) of
        (HalfDay _ _ Worked, Nothing) -> throwM $ ModelException errDbInconsistency
        (HalfDay _ _ Holiday, Just _) -> throwM $ ModelException errDbInconsistency
        (_, _)                        -> return (hd, mbHdw)

hdwSetOffice :: (MonadIO m, MonadCatch m) => Day -> TimeInDay -> Office -> SqlPersistT m ()
hdwSetOffice day tid office = do
    (_, Entity hdwId _, _) <- hdHdwProjGetInt day tid
    update hdwId [HalfDayWorkedOffice =. office]

hdwSetNotes :: (MonadIO m, MonadCatch m) => Day -> TimeInDay -> Text -> SqlPersistT m ()
hdwSetNotes day tid notes = do
    (_, Entity hdwId _, _) <- hdHdwProjGetInt day tid
    update hdwId [HalfDayWorkedNotes =. notes]

hdwSetProject :: (MonadIO m, MonadCatch m) => Day -> TimeInDay -> Project -> SqlPersistT m () 
hdwSetProject day tid project = do
    (_, Entity hdwId _, _) <- hdHdwProjGetInt day tid
    pId <- projGetInt project
    update hdwId [HalfDayWorkedProjectId =. pId]

hdwSetArrived 
    :: (MonadIO m, MonadCatch m) 
    => Day 
    -> TimeInDay 
    -> TimeOfDay 
    -> SqlPersistT m () 
hdwSetArrived day tid tod = do
    (eHd, eHdw, _) <- hdHdwProjGetInt day tid
    editTime eHd eHdw $ setArrived tod
  where setArrived tod' hdw = hdw { halfDayWorkedArrived = tod' }

hdwSetLeft 
    :: (MonadIO m, MonadCatch m) 
    => Day 
    -> TimeInDay 
    -> TimeOfDay 
    -> SqlPersistT m () 
hdwSetLeft day tid tod = do
    (eHd, eHdw, _) <- hdHdwProjGetInt day tid
    editTime eHd eHdw $ setLeft tod
  where setLeft tod' hdw = hdw { halfDayWorkedLeft = tod' }

hdwSetArrivedAndLeft 
    :: (MonadIO m, MonadCatch m) 
    => Day 
    -> TimeInDay 
    -> TimeOfDay 
    -> TimeOfDay 
    -> SqlPersistT m () 
hdwSetArrivedAndLeft day tid tArrived tLeft = do
    (eHd, eHdw, _) <- hdHdwProjGetInt day tid
    editTime eHd eHdw $ setArrivedAndLeft tArrived tLeft
  where setArrivedAndLeft arrived' left' hdw = 
            hdw { halfDayWorkedArrived = arrived'
                , halfDayWorkedLeft    = left' }

hdSetHoliday :: (MonadIO m, MonadCatch m) => Day -> TimeInDay -> SqlPersistT m () 
hdSetHoliday day tid = try (hdGetInt day tid) >>=
    \case 
        -- Create a new entry
        Left (ModelException _) -> void $ insert $ HalfDay day tid Holiday
        -- Edit existing entry
        Right (Entity hdId _)   -> do          
            -- Delete entry from HalfDayWorked if it exists
            deleteWhere [HalfDayWorkedHalfDayId ==. hdId]
            -- Update entry
            update hdId [HalfDayType =. Holiday]

hdSetWork 
    :: (MonadIO m, MonadCatch m) 
    => Day 
    -> TimeInDay 
    -> Project 
    -> SqlPersistT m () 
hdSetWork day tid project = do 
    projId <- projGetInt project
    eiHd <- try $ hdGetInt day tid
    hdId <- case eiHd of
        -- Create a new entry
        Left (ModelException _) -> insert $ HalfDay day tid Worked 
        -- Edit existing entry
        Right (Entity hdId _)   -> do
          -- Update entry
          update hdId [HalfDayType =. Worked]
          return hdId
    void $ insert $ HalfDayWorked "" tArrived tLeft Rennes projId hdId
  where 
    tArrived = if tid == Morning then TimeOfDay 9 0 0 else TimeOfDay 13 30 0 
    tLeft = if tid == Morning then TimeOfDay 12 0 0 else TimeOfDay 17 30 0 
   
hdRm :: (MonadIO m, MonadCatch m) => Day -> TimeInDay -> SqlPersistT m () 
hdRm day tid = do
    (Entity hdId _) <- hdGetInt day tid
    -- Delete entry from HalfDayWorked if it exists
    deleteWhere [HalfDayWorkedHalfDayId ==. hdId]
    delete hdId
 
-- Internal project functions

-- Get a half day if it exists or raise an exception
hdGetInt :: (MonadIO m, MonadThrow m) => Day -> TimeInDay -> SqlPersistT m (Entity HalfDay)
hdGetInt day tid = getBy (DayAndTimeInDay day tid) >>=
    \case 
        Nothing -> throwM $ ModelException $ errHdNotFound day tid
        Just e  -> return e

hdwProjGetInt 
    :: (MonadIO m, MonadThrow m) 
    => HalfDayId 
    -> SqlPersistT m (Entity HalfDayWorked, Entity Project)
hdwProjGetInt hdId = getBy (UniqueHalfDayId hdId) >>=
    \case
        Nothing -> throwM $ ModelException $ errHdwIdNotFound hdId
        Just e@(Entity _ (HalfDayWorked _ _ _ _ pId _)) -> do
            mbProj <- get pId
            project <- case mbProj of 
                Nothing -> throwM $ ModelException $ errProjIdNotFound pId
                Just p  -> return (Entity pId p)
            return (e, project)

hdHdwProjGetInt 
    :: (MonadIO m, MonadThrow m) 
    => Day 
    -> TimeInDay 
    -> SqlPersistT m (Entity HalfDay, Entity HalfDayWorked, Entity Project)
hdHdwProjGetInt day tid = do
    eHd@(Entity hdId _) <- hdGetInt day tid
    (eHdw, eProj) <- hdwProjGetInt hdId
    return (eHd, eHdw, eProj)

-- hd private functions

-- Check that the constraints on the times are valid between the two days
timesAreOrderedInDay 
    :: TimeInDay 
    -> HalfDayWorked 
    -> Maybe HalfDayWorked 
    -> Maybe Text
timesAreOrderedInDay Morning hdw mbOtherHdw = 
    timeAreOrdered $ timesOfDay hdw ++ otherTimes
  where otherTimes = concatMap timesOfDay mbOtherHdw
-- We switch the arguments and call the same function
timesAreOrderedInDay Afternoon hdw (Just otherHdw) =
    timesAreOrderedInDay Morning otherHdw (Just hdw)
-- Afternoon only, just need to check for half day
timesAreOrderedInDay Afternoon hdw Nothing = timeAreOrdered $ timesOfDay hdw

-- Set arrived/left time
editTime :: (MonadIO m, MonadCatch m)
    => Entity HalfDay
    -> Entity HalfDayWorked
    -> (HalfDayWorked -> HalfDayWorked)
    -> SqlPersistT m ()
editTime (Entity _ (HalfDay day tid _)) (Entity hdwId hdw) setTime = do
    eiHdHdwProj <- try $ hdHdwProjGetInt day $ other tid
    let mbOtherHdw = case eiHdHdwProj of
          Left (ModelException _)     -> Nothing
          Right (_, Entity _ oHdw, _) -> Just oHdw
    -- Apply time to arrived/left hdw
    let hdw' = setTime hdw
    -- Check if it works
    case timesAreOrderedInDay tid hdw' mbOtherHdw of
        Just msg -> throwM $ ModelException msg
        Nothing  -> replace hdwId hdw'

-- Return the times in the day in a list
timesOfDay :: HalfDayWorked -> [TimeOfDay]
timesOfDay hdw = [halfDayWorkedArrived hdw, halfDayWorkedLeft hdw]

-- Return true if the list is sorted
isOrdered :: (Ord a) => [a] -> Bool
isOrdered []       = True
isOrdered [_]      = True
isOrdered (x:y:xs) = x <= y && isOrdered (y:xs)

-- Return Nothing if the times in the list are ordered. Return an error message
-- otherwise
timeAreOrdered :: [TimeOfDay] -> Maybe Text
timeAreOrdered times = if isOrdered times
    then Nothing
    else Just errTimesAreWrong

