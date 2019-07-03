-- | This module reexport the public functions from "Internal.Model"
module Model
    ( 
    -- * Rexexported from Internal.Model
      EntityField
        ( HalfDayDay
        , HalfDayId
        , HalfDayTimeInDay
        , HalfDayType
        , HalfDayWorkedHalfDayId
        , HalfDayWorkedNotes 
        , HalfDayWorkedOffice
        , HalfDayWorkedProjectId
        , ProjectId
        , ProjectName
        )
    , HalfDay(..)
    , HalfDayId
    , HalfDayWorked(..)
    , NewModel.Notes
    , NewModel.ProjName
    , NewModel.Project
    , NewModel.mkNotes
    , NewModel.mkNotesLit
    , NewModel.mkProject
    , NewModel.mkProjectLit
    , NewModel.unNotes
    , NewModel.unProject
    , ProjectId
    , Unique(DayAndTimeInDay, UniqueHalfDayId, UniqueName)
    , migrateAll
    , BadArgument(..)
    , ProjExists(..)
    , ProjHasHDW(..)
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
    , selectFirst
    , update
    , (=.)
    )
import qualified Database.Persist.Sqlite as P ((==.))

import           Data.Maybe (isJust)
import           Formatting (int, left, sformat, (%.))

import           HalfDayType(HalfDayType(..))
import qualified NewModel
import           Office (Office(..))
import           TimeInDay(TimeInDay(..), other)

import           Internal.Model

-- Exceptions that are likely to occure

-- | The requested project has not been found
newtype ProjNotFound = ProjNotFound NewModel.Project

instance Exception ProjNotFound

instance Show ProjNotFound where
    show (ProjNotFound project) = "The project " <> name <> " is not in the database"
      where name = Text.unpack (NewModel.unProject project)

-- | A project with the same name allready exists in the db
newtype ProjExists = ProjExists NewModel.Project

instance Exception ProjExists

instance Show ProjExists where
    show (ProjExists project) = "The project " <> name <> " exists in the database"
      where name = Text.unpack (NewModel.unProject project)

-- | The project has associated hdw
newtype ProjHasHDW = ProjHasHDW NewModel.Project

instance Exception ProjHasHDW

instance Show ProjHasHDW where
    show (ProjHasHDW project) = "The project " <> name <> " has associated half-day work"
      where name = Text.unpack (NewModel.unProject project)

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

-- | Inconsistency in the DB
data DbInconsistency = DbInconsistency

instance Exception DbInconsistency

instance Show DbInconsistency where
    show _ = "Warning db inconsistency"

-- | Bad argument for function
data BadArgument = BadArgument

instance Exception BadArgument

instance Show BadArgument where
    show _ = "Bad argument"

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
projExists :: MonadIO m => NewModel.Project -> SqlPersistT m Bool
projExists project = isJust <$> getBy (UniqueName $ NewModel.unProject project)


-- | Add a project 
projAdd :: (MonadIO m) => NewModel.Project -> SqlPersistT m ()
projAdd project = do
    guardProjNotExistsInt project
    void $ insert $ projectToDb project

-- | Get the list of the projects present in the database
projList :: MonadIO m => SqlPersistT m [NewModel.Project]
projList = map (dbToProject . entityVal) <$> selectList [] [Asc ProjectName] 

-- | Delete a project 
projRm :: (MonadIO m) => NewModel.Project -> SqlPersistT m ()
projRm project = do
    -- The following can throw exception same exception apply to this function
    -- so we dont catch it here
    pId <- projGetInt project 
    -- Test if there is hdw using this project
    selectFirst [HalfDayWorkedProjectId P.==. pId] [] >>=
        \case
            Nothing -> delete pId
            Just _  -> throwIO $ ProjHasHDW project

-- | Rename a project 
projRename :: (MonadIO m) => NewModel.Project -> NewModel.Project -> SqlPersistT m ()
projRename p1 p2 = do
    pId <- projGetInt p1
    guardProjNotExistsInt p2
    replace pId $ projectToDb p2

-- Internal project functions

-- | Get a project with error handling
projGetInt :: (MonadIO m) => NewModel.Project -> SqlPersistT m (Key Project)
projGetInt project = getBy (UniqueName $ NewModel.unProject project) >>= 
    \case
        Nothing             -> throwIO $ ProjNotFound project
        Just (Entity pId _) -> return pId

-- | Guard to check if a project is allready present in the db. If so, raise an
-- exception
guardProjNotExistsInt :: (MonadIO m) => NewModel.Project -> SqlPersistT m ()
guardProjNotExistsInt project = do
    exists <- projExists project
    when exists (throwIO $ ProjExists project)

-- Exported hd functions

-- | This is the main request function
hdHdwProjGet
    :: (MonadIO m, MonadUnliftIO m) 
    => Time.Day
    -> TimeInDay 
    -> SqlPersistT m (HalfDay, Maybe (HalfDayWorked, NewModel.Project))
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
                -> return (hd, Just (hdw, dbToProject proj))
            _ -> throwIO DbInconsistency

-- | Set the office for a day-time in day
hdwSetOffice :: (MonadIO m) => Time.Day -> TimeInDay -> Office -> SqlPersistT m ()
hdwSetOffice day tid office = do
    (_, Entity hdwId _, _) <- hdHdwProjGetInt day tid
    update hdwId [HalfDayWorkedOffice =. office]

-- | Set the notes for a day-time in day
hdwSetNotes :: (MonadIO m) => Time.Day -> TimeInDay -> NewModel.Notes -> SqlPersistT m ()
hdwSetNotes day tid notes = do
    (_, Entity hdwId _, _) <- hdHdwProjGetInt day tid
    update hdwId [HalfDayWorkedNotes =. NewModel.unNotes notes]

-- | Set a work half-day with a project
hdwSetProject :: (MonadIO m) => Time.Day -> TimeInDay -> NewModel.Project -> SqlPersistT m () 
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
    (_, Entity hdwId hdw, _) <- hdHdwProjGetInt day tid
    let hdw' = hdw { halfDayWorkedArrived = tod }
    guardNewTimesAreOk day tid hdw' 
    replace hdwId hdw'

-- | Set left time for a working half-day
hdwSetLeft 
    :: (MonadIO m, MonadUnliftIO m) 
    => Time.Day 
    -> TimeInDay 
    -> Time.TimeOfDay 
    -> SqlPersistT m () 
hdwSetLeft day tid tod = do
    (_, (Entity hdwId hdw), _) <- hdHdwProjGetInt day tid
    let hdw' = hdw { halfDayWorkedLeft = tod }
    guardNewTimesAreOk day tid hdw' 
    replace hdwId hdw'

-- | Set both arrived and left time for a working half-day
hdwSetArrivedAndLeft 
    :: (MonadIO m, MonadUnliftIO m) 
    => Time.Day 
    -> TimeInDay 
    -> Time.TimeOfDay 
    -> Time.TimeOfDay 
    -> SqlPersistT m () 
hdwSetArrivedAndLeft day tid tArrived tLeft = do
    (_, (Entity hdwId hdw), _) <- hdHdwProjGetInt day tid
    let hdw' = hdw { halfDayWorkedArrived = tArrived
                   , halfDayWorkedLeft    = tLeft }
    guardNewTimesAreOk day tid hdw' 
    replace hdwId hdw'

-- | Set a half-day as holiday
hdSetHoliday 
    :: (MonadIO m, MonadUnliftIO m) 
    => Time.Day 
    -> TimeInDay 
    -> HalfDayType
    -> SqlPersistT m () 
hdSetHoliday _ _ Worked = throwIO BadArgument
hdSetHoliday day tid hdt = try (hdGetInt day tid) >>=
    \case 
        -- Create a new entry
        Left (HdNotFound _ _) -> void $ insert $ HalfDay day tid hdt
        -- Edit existing entry
        Right (Entity hdId _)   -> do
            -- Delete entry from HalfDayWorked if it exists
            deleteWhere [HalfDayWorkedHalfDayId P.==. hdId]
            -- Update entry
            update hdId [HalfDayType =. hdt]

-- | Set a half-day as working on a project. 
hdSetWork 
    :: (MonadIO m, MonadUnliftIO m) 
    => Time.Day 
    -> TimeInDay 
    -> NewModel.Project 
    -> Office
    -> Time.TimeOfDay
    -> Time.TimeOfDay
    -> SqlPersistT m () 
hdSetWork day tid project office tArrived tLeft = do 
    projId <- projGetInt project
    eiHd <- try $ hdGetInt day tid
    hdId <- case eiHd of
        -- Create a new entry
        Left (HdNotFound _ _) -> insert $ HalfDay day tid Worked 
        -- Edit existing entry
        Right (Entity hdId _) -> do
          -- Update entry
          update hdId [HalfDayType =. Worked]
          return hdId
    let hdw = HalfDayWorked "" tArrived tLeft office projId hdId
    guardNewTimesAreOk day tid hdw
    void $ insert hdw
   
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

-- | Make sure that the times in hdw are ok:
--   - arrived < left
--   - consistent with the potential other hdw
guardNewTimesAreOk :: (MonadIO m, MonadUnliftIO m)
    => Time.Day
    -> TimeInDay
    -> HalfDayWorked
    -> SqlPersistT m ()
guardNewTimesAreOk day tid hdw = do
    eiHdHdwProj <- try $ hdHdwProjGetInt day $ other tid
    let mbOtherHdw = case eiHdHdwProj of
          Left (HdwNotFound _ _)      -> Nothing
          Right (_, Entity _ oHdw, _) -> Just oHdw
    -- Check if it works
    unless (timesAreOrderedInDay tid hdw mbOtherHdw) (throwIO $ TimesAreWrong)

-- | Return the times in the day in a list
timesOfDay :: HalfDayWorked -> [Time.TimeOfDay]
timesOfDay hdw = [halfDayWorkedArrived hdw, halfDayWorkedLeft hdw]

-- | Return true if the list is sorted
isOrdered :: (Ord a) => [a] -> Bool
isOrdered []       = True
isOrdered [_]      = True
isOrdered (x:y:xs) = x <= y && isOrdered (y:xs)

