-- | The main API
module Db.Model
    ( -- * Exceptions
      ProjExists (..),
      ProjHasHd (..),
      ProjNotFound (..),
      HdNotFound (..),
      TimesAreWrong (..),
      UserExists (..),
      UserNotFound (..),
      renderUserError,

      -- * Half-day functions
      hdGet,
      hdRm,
      hdSetOff,
      hdSetWork,
      hdSetArrived,
      hdSetArrivedAndLeft,
      hdSetLeft,
      hdSetNotes,
      hdSetOffice,
      hdSetProject,

      -- * Other query functions
      monthGet,
      weekGet,

      -- * Project functions
      projAdd,
      projExists,
      projList,
      projRename,
      projRm,

      -- * User functions
      userAdd,
      userChangePassword,
      userCheck,
      userExists,
      userList,
      userRename,
      userRm,

      -- * Misc
      cleanDB,
      migrateAll,
    )
where

import Control.Monad (void, when)
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except.Extra (firstExceptT, hoistMaybe)
import Crypto.KDF.BCrypt (hashPassword, validatePassword)
import Crypto.Random.Types (MonadRandom)
import Data.Maybe (isJust)
import Data.WorldPeace (Contains, OpenUnion, catchesOpenUnion, openUnionLift, relaxOpenUnion)
import Database.Esqueleto
    ( (&&.),
      (<=.),
      (==.),
      (>=.),
      (?.),
      LeftOuterJoin (..),
      (^.),
      asc,
      desc,
      from,
      just,
      on,
      orderBy,
      select,
      val,
      where_,
    )
import Database.Persist
    ( (=.),
      Entity (..),
      Filter,
      Key,
      SelectOpt (Asc),
      delete,
      deleteWhere,
      getBy,
      insert,
      replace,
      selectFirst,
      selectList,
      update,
    )
import qualified Database.Persist as P ((==.))
import Database.Persist.Sql (SqlPersistT, toSqlKey)
import Db.HalfDay (HalfDay (..))
import Db.Internal.Convert
    ( dbToOff,
      dbToProject,
      dbToWorked,
      offDayTypeToDb,
      projectToDb,
    )
import Db.Internal.DBHalfDayType (DBHalfDayType (..))
import Db.Internal.DBModel
import Db.Login (Login, mkLogin, unLogin)
import Db.Month (Month (..), firstDay, lastDay)
import qualified Db.MonthF as MonthF (MonthWithDays, add, empty)
import Db.Notes (Notes, unNotes)
import Db.OffDayType (OffDayType (..))
import Db.Office (Office (..))
import Db.Password (Password, unPassword)
import Db.Project (Project, unProject)
import Db.TimeInDay (TimeInDay (..), other)
import Db.Week (Week (..), monday, sunday)
import qualified Db.WeekF as WeekF (WeekWithDays, add, empty)
import RIO hiding ((^.), on)
import RIO.List as L (headMaybe)
import qualified RIO.Text as Text (pack, unpack)
import qualified RIO.Time as Time
    ( Day,
      TimeOfDay (..),
    )

-- Exceptions that are likely to occure

-- | The requested project has not been found
newtype ProjNotFound = ProjNotFound Project

instance Show ProjNotFound where
    show (ProjNotFound project) = "The project " <> name <> " is not in the database"
        where
            name = Text.unpack (unProject project)

-- | A project with the same name already exists in the db
newtype ProjExists = ProjExists Project

instance Show ProjExists where
    show (ProjExists project) = "The project " <> name <> " exists in the database"
        where
            name = Text.unpack (unProject project)

-- | The project has associated hds
newtype ProjHasHd = ProjHasHd Project

instance Show ProjHasHd where
    show (ProjHasHd project) = "The project " <> name <> " has associated half-day work"
        where
            name = Text.unpack (unProject project)

-- | Requested user does not exist
newtype UserNotFound = UserNotFound Login

instance Show UserNotFound where
    show (UserNotFound login) = "The user " <> Text.unpack (unLogin login) <> " is not in the database"

-- | A user with the same login already exists in the db
newtype UserExists = UserExists Login

instance Show UserExists where
    show (UserExists login) = "The user " <> Text.unpack (unLogin login) <> " exists in the database"

-- | There is no record for specified half-day
data HdNotFound = HdNotFound Time.Day TimeInDay

instance Show HdNotFound where
    show (HdNotFound day tid) = "Nothing for " <> Text.unpack (textDisplay day) <> " " <> show tid

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

-- | Clean up the db
cleanDB :: (MonadIO m) => SqlPersistT m ()
cleanDB = do
    deleteWhere ([] :: [Filter DBHalfDayWorked])
    deleteWhere ([] :: [Filter DBHalfDay])
    deleteWhere ([] :: [Filter DBProject])
    deleteWhere ([] :: [Filter DBUser])

-- Exported user functions
renderUserError ::
    Contains err '[UserNotFound, UserExists] =>
    OpenUnion err ->
    Text
renderUserError = renderUserError' . relaxOpenUnion
    where
        renderUserError' :: OpenUnion '[UserNotFound, UserExists] -> Text
        renderUserError' = catchesOpenUnion (renderUserNotFound, renderUserExists)
        renderUserNotFound :: UserNotFound -> Text
        renderUserNotFound = Text.pack . show
        renderUserExists :: UserExists -> Text
        renderUserExists = Text.pack . show

-- | Check the password of a user
userCheck ::
    (MonadIO m) =>
    Login ->
    Password ->
    ExceptT (OpenUnion '[UserNotFound]) (SqlPersistT m) Bool
userCheck login password = do
    (Entity _ (DBUser _ hash)) <- userGetInt login
    pure $ validatePassword (encodeUtf8 $ unPassword password) (encodeUtf8 hash)

-- | Add a new user
userAdd ::
    (MonadIO m) =>
    Login ->
    Password ->
    Int ->
    ExceptT (OpenUnion '[UserExists]) (SqlPersistT m) ()
userAdd login password cost = do
    guardUserNotExistsInt login
    hash <- liftIO $ hashTxt password cost
    lift . void $ insert (DBUser (unLogin login) hash)

-- | Check if a user exists in the DB
userExists :: (MonadIO m) => Login -> SqlPersistT m Bool
userExists login = isJust <$> getBy (UniqueLogin $ unLogin login)

-- | Delete a user
userRm ::
    (MonadIO m) =>
    Login ->
    ExceptT (OpenUnion '[UserNotFound]) (SqlPersistT m) ()
userRm login = userGetInt login >>= lift . delete . entityKey

-- | Display the list of users
userList :: (MonadIO m) => SqlPersistT m [Login]
userList = mapMaybe (mkLogin . dBUserLogin . entityVal) <$> selectList [] [Asc DBUserLogin]

userRename ::
    (MonadIO m) =>
    Login ->
    Login ->
    ExceptT (OpenUnion '[UserNotFound, UserExists]) (SqlPersistT m) ()
userRename login1 login2 = do
    firstExceptT relaxOpenUnion $ guardUserNotExistsInt login2
    (Entity uId _) <- firstExceptT relaxOpenUnion $ userGetInt login1
    lift $ update uId [DBUserLogin =. unLogin login2]

-- | Change the password for a user
userChangePassword ::
    (MonadIO m) =>
    Login ->
    Password ->
    Int ->
    ExceptT (OpenUnion '[UserNotFound]) (SqlPersistT m) ()
userChangePassword login password cost = do
    (Entity uId _) <- userGetInt login
    hash <- liftIO $ hashTxt password cost
    lift $ update uId [DBUserHash =. hash]

-- Private user functions

-- | Get a user by its login
userGetInt ::
    (MonadIO m) =>
    Login ->
    ExceptT (OpenUnion '[UserNotFound]) (SqlPersistT m) (Entity DBUser)
userGetInt login = do
    mbEntity <- lift $ getBy (UniqueLogin $ unLogin login)
    hoistMaybe (openUnionLift $ UserNotFound login) mbEntity

-- | Throw an exception if a user already exists
guardUserNotExistsInt ::
    (MonadIO m) =>
    Login ->
    ExceptT (OpenUnion '[UserExists]) (SqlPersistT m) ()
guardUserNotExistsInt login = do
    exists <- lift $ userExists login
    when exists (throwError . openUnionLift $ UserExists login)

-- | Hash function operating on 'Text'
hashTxt :: MonadRandom m => Password -> Int -> m Text
hashTxt password cost = do
    hash <- hashPassword cost (encodeUtf8 $ unPassword password)
    pure $ decodeUtf8With lenientDecode hash

-- Exported project functions

-- | Check if a project exists
projExists :: MonadIO m => Project -> SqlPersistT m Bool
projExists project = isJust <$> getBy (UniqueName $ unProject project)

-- | Add a project
projAdd ::
    (MonadIO m) =>
    Project ->
    ExceptT (OpenUnion '[ProjExists]) (SqlPersistT m) ()
projAdd project = do
    guardProjNotExistsInt project
    lift . void . insert $ projectToDb project

-- | Get the list of the projects present in the database
projList :: MonadIO m => SqlPersistT m [Project]
projList = mapMaybe (dbToProject . entityVal) <$> selectList [] [Asc DBProjectName]

-- | Delete a project
projRm ::
    (MonadIO m) =>
    Project ->
    ExceptT (OpenUnion '[ProjNotFound, ProjHasHd]) (SqlPersistT m) ()
projRm project = do
    -- The following can throw exception same exception apply to this function
    -- so we dont catch it here
    pId <- firstExceptT relaxOpenUnion $ projGetInt project
    -- Test if there is hdw using this project
    mbHasHd <- lift (selectFirst [DBHalfDayWorkedProjectId P.==. pId] [])
    case mbHasHd of
        Nothing -> lift $ delete pId
        Just _ -> throwError $ openUnionLift $ ProjHasHd project

-- | Rename a project
projRename ::
    (MonadIO m) =>
    Project ->
    Project ->
    ExceptT (OpenUnion '[ProjNotFound, ProjExists]) (SqlPersistT m) ()
projRename p1 p2 = do
    pId <- firstExceptT relaxOpenUnion $ projGetInt p1
    firstExceptT relaxOpenUnion $ guardProjNotExistsInt p2
    lift $ replace pId $ projectToDb p2

-- Internal project functions

-- | Get a project with error handling
projGetInt ::
    (MonadIO m) =>
    Project ->
    ExceptT (OpenUnion '[ProjNotFound]) (SqlPersistT m) (Key DBProject)
projGetInt project = do
    mbEntity <- lift $ getBy (UniqueName $ unProject project)
    let mbId = entityKey <$> mbEntity
    hoistMaybe (openUnionLift $ ProjNotFound project) mbId

-- | Guard to check if a project is already present in the db. If so, raise an
-- exception
guardProjNotExistsInt ::
    (MonadIO m) =>
    Project ->
    ExceptT (OpenUnion '[ProjExists]) (SqlPersistT m) ()
guardProjNotExistsInt project = do
    exists <- lift $ projExists project
    when exists (throwError . openUnionLift $ ProjExists project)

-- Exported hd functions

-- | This is the main request function
hdGet ::
    (MonadIO m, MonadUnliftIO m) =>
    Time.Day ->
    TimeInDay ->
    ExceptT (OpenUnion '[HdNotFound]) (SqlPersistT m) HalfDay
hdGet day tid =
    lift
        ( select $ from $ \(hd `LeftOuterJoin` mbHdw `LeftOuterJoin` mbProj) -> do
              where_
                  ( hd ^. DBHalfDayDay ==. val day
                        &&. hd ^. DBHalfDayTimeInDay ==. val tid
                  )
              on (mbProj ?. DBProjectId ==. mbHdw ?. DBHalfDayWorkedProjectId)
              on (just (hd ^. DBHalfDayId) ==. mbHdw ?. DBHalfDayWorkedHalfDayId)
              pure (hd, mbHdw, mbProj)
        )
        >>= \case
            [] -> (throwError . openUnionLift) $ HdNotFound day tid
            (x : _) -> dbToHalfDayInt x

-- | Get the half-days on a complete week
weekGet ::
    (MonadIO m, MonadUnliftIO m) =>
    Week ->
    SqlPersistT m WeekF.WeekWithDays
weekGet week = do
    hdList <- rangeGet (monday week) (sunday week)
    pure $ toWeekF hdList
    where
        toWeekF = foldr (\hd fw -> fromMaybe fw $ WeekF.add hd fw) (WeekF.empty Nothing week)

monthGet ::
    (MonadIO m, MonadUnliftIO m) =>
    Month ->
    SqlPersistT m MonthF.MonthWithDays
monthGet month = do
    hdList <- rangeGet (firstDay month) (lastDay month)
    pure $ toMonthF hdList
    where
        toMonthF = foldr (\hd fm -> fromMaybe fm $ MonthF.add hd fm) (MonthF.empty Nothing month)

-- | Set the office for a day-time in day
hdSetOffice ::
    (MonadIO m) =>
    Time.Day ->
    TimeInDay ->
    Office ->
    ExceptT (OpenUnion '[HdNotFound]) (SqlPersistT m) ()
hdSetOffice day tid office = do
    (_, Entity hdwId _, _) <- hdHdwProjGetInt day tid
    lift $ update hdwId [DBHalfDayWorkedOffice =. office]

-- | Set the notes for a day-time in day
hdSetNotes :: (MonadIO m) => Time.Day -> TimeInDay -> Notes -> ExceptT (OpenUnion '[HdNotFound]) (SqlPersistT m) ()
hdSetNotes day tid notes = do
    (_, Entity hdwId _, _) <- hdHdwProjGetInt day tid
    lift $ update hdwId [DBHalfDayWorkedNotes =. unNotes notes]

-- | Set a work half-day with a project
hdSetProject ::
    (MonadIO m) =>
    Time.Day ->
    TimeInDay ->
    Project ->
    ExceptT (OpenUnion '[HdNotFound, ProjNotFound]) (SqlPersistT m) ()
hdSetProject day tid project = do
    (_, Entity hdwId _, _) <- firstExceptT relaxOpenUnion $ hdHdwProjGetInt day tid
    pId <- firstExceptT relaxOpenUnion $ projGetInt project
    lift $ update hdwId [DBHalfDayWorkedProjectId =. pId]

-- | Set arrived time for a working half-day
hdSetArrived ::
    (MonadIO m, MonadUnliftIO m) =>
    Time.Day ->
    TimeInDay ->
    Time.TimeOfDay ->
    ExceptT (OpenUnion '[HdNotFound, TimesAreWrong]) (SqlPersistT m) ()
hdSetArrived day tid tod = do
    (_, Entity hdwId hdw, _) <- firstExceptT relaxOpenUnion $ hdHdwProjGetInt day tid
    let hdw' = hdw {dBHalfDayWorkedArrived = tod}
    firstExceptT relaxOpenUnion $ guardNewTimesAreOk day tid hdw'
    lift $ replace hdwId hdw'

-- | Set left time for a working half-day
hdSetLeft ::
    (MonadIO m, MonadUnliftIO m) =>
    Time.Day ->
    TimeInDay ->
    Time.TimeOfDay ->
    ExceptT (OpenUnion '[HdNotFound, TimesAreWrong]) (SqlPersistT m) ()
hdSetLeft day tid tod = do
    (_, Entity hdwId hdw, _) <- firstExceptT relaxOpenUnion $ hdHdwProjGetInt day tid
    let hdw' = hdw {dBHalfDayWorkedLeft = tod}
    firstExceptT relaxOpenUnion $ guardNewTimesAreOk day tid hdw'
    lift $ replace hdwId hdw'

-- | Set both arrived and left times for a working half-day
hdSetArrivedAndLeft ::
    (MonadIO m, MonadUnliftIO m) =>
    Time.Day ->
    TimeInDay ->
    Time.TimeOfDay ->
    Time.TimeOfDay ->
    ExceptT (OpenUnion '[HdNotFound, TimesAreWrong]) (SqlPersistT m) ()
hdSetArrivedAndLeft day tid tArrived tLeft = do
    (_, Entity hdwId hdw, _) <- firstExceptT relaxOpenUnion $ hdHdwProjGetInt day tid
    let hdw' =
            hdw
                { dBHalfDayWorkedArrived = tArrived,
                  dBHalfDayWorkedLeft = tLeft
                }
    firstExceptT relaxOpenUnion $ guardNewTimesAreOk day tid hdw'
    lift $ replace hdwId hdw'

-- | Set a half-day as off
hdSetOff ::
    (MonadIO m, MonadUnliftIO m) =>
    Time.Day ->
    TimeInDay ->
    OffDayType ->
    SqlPersistT m ()
hdSetOff day tid hdt = do
    eiHd <- runExceptT $ hdGetInt day tid
    case eiHd of
        Left (_ :: OpenUnion '[HdNotFound]) -> void $ insert $ DBHalfDay day tid dbHdt
        -- Edit existing entry
        Right (Entity hdId _) -> do
            -- Delete entry from HalfDayWorked if it exists
            deleteWhere [DBHalfDayWorkedHalfDayId P.==. hdId]
            -- Update entry
            update hdId [DBHalfDayType =. dbHdt]
    where
        dbHdt = offDayTypeToDb hdt

-- | Set a half-day as working on a project.
hdSetWork ::
    (MonadIO m, MonadUnliftIO m) =>
    Time.Day ->
    TimeInDay ->
    Project ->
    Office ->
    Time.TimeOfDay ->
    Time.TimeOfDay ->
    ExceptT (OpenUnion '[ProjNotFound, TimesAreWrong]) (SqlPersistT m) ()
hdSetWork day tid project office tArrived tLeft = do
    -- Get the proj entry if it exists
    projId <- firstExceptT relaxOpenUnion $ projGetInt project
    let hdw = DBHalfDayWorked "" tArrived tLeft office (toSqlKey 0) (toSqlKey 0)
    firstExceptT relaxOpenUnion $ guardNewTimesAreOk day tid hdw
    eiHd <- lift . runExceptT $ hdGetInt day tid
    hdId <- case eiHd of
        -- Create a new entry
        Left (_ :: OpenUnion '[HdNotFound]) -> lift $ insert $ DBHalfDay day tid DBWorked
        -- Edit existing entry
        Right (Entity hdId _) -> do
            -- Update entry
            lift $ update hdId [DBHalfDayType =. DBWorked]
            pure hdId
    let hdw' =
            hdw
                { dBHalfDayWorkedProjectId = projId,
                  dBHalfDayWorkedHalfDayId = hdId
                }
    mbEntity <- lift $ getBy (UniqueHalfDayId hdId)
    case mbEntity of
        Nothing -> lift $ void $ insert hdw'
        Just (Entity hdwId _) -> lift $ replace hdwId hdw'

-- | Remove a half-day from the db
hdRm ::
    (MonadIO m) =>
    Time.Day ->
    TimeInDay ->
    ExceptT (OpenUnion '[HdNotFound]) (SqlPersistT m) ()
hdRm day tid = do
    (Entity hdId _) <- hdGetInt day tid
    -- Delete entry from HalfDayWorked if it exists
    lift $ deleteWhere [DBHalfDayWorkedHalfDayId P.==. hdId]
    lift $ delete hdId

-- Internal project functions

-- | Get a half day if it exists or raise an exception
hdGetInt ::
    (MonadIO m) =>
    Time.Day ->
    TimeInDay ->
    ExceptT (OpenUnion '[HdNotFound]) (SqlPersistT m) (Entity DBHalfDay)
hdGetInt day tid = do
    mbHd <- lift $ getBy (DayAndTimeInDay day tid)
    hoistMaybe (openUnionLift $ HdNotFound day tid) mbHd

-- | Private function to get a half-day work along with the project from a day
-- and a time in day
hdHdwProjGetInt ::
    (MonadIO m) =>
    Time.Day ->
    TimeInDay ->
    ExceptT (OpenUnion '[HdNotFound]) (SqlPersistT m) (Entity DBHalfDay, Entity DBHalfDayWorked, Entity DBProject)
hdHdwProjGetInt day tid = do
    hdHdwProjs <- lift $ select $ from $ \(hd, hdw, proj) -> do
        where_
            ( hd ^. DBHalfDayDay ==. val day
                  &&. hd ^. DBHalfDayTimeInDay ==. val tid
                  &&. hdw ^. DBHalfDayWorkedProjectId ==. proj ^. DBProjectId
                  &&. hdw ^. DBHalfDayWorkedHalfDayId ==. hd ^. DBHalfDayId
            )
        pure (hd, hdw, proj)
    case L.headMaybe hdHdwProjs of
        Nothing -> throwError $ openUnionLift $ HdNotFound day tid
        Just hd -> pure hd

-- | Convert output of a join query to a 'HalfDay'. Throw an exception in case
-- of inconsistencies in the DB along the way
dbToHalfDayInt ::
    MonadIO m =>
    (Entity DBHalfDay, Maybe (Entity DBHalfDayWorked), Maybe (Entity DBProject)) ->
    m HalfDay
dbToHalfDayInt (Entity _ hd, Nothing, Nothing) =
    case dbToOff hd of
        Nothing -> throwIO DbInconsistency
        Just off -> pure $ MkHalfDayOff off
dbToHalfDayInt (Entity _ hd, Just (Entity _ hdw), Just (Entity _ proj)) =
    case dbToWorked hd hdw proj of
        Nothing -> throwIO DbInconsistency
        Just worked -> pure $ MkHalfDayWorked worked
dbToHalfDayInt _ = throwIO DbInconsistency

-- hd private functions

-- | Get a list of 'HalfDay' between a range of two 'Day'
rangeGet :: (MonadIO m, MonadUnliftIO m) => Time.Day -> Time.Day -> SqlPersistT m [HalfDay]
rangeGet day1 day2 = do
    tupleList <- select $ from $ \(hd `LeftOuterJoin` mbHdw `LeftOuterJoin` mbProj) -> do
        where_
            ( hd ^. DBHalfDayDay >=. val day1
                  &&. hd ^. DBHalfDayDay <=. val day2
            )
        on (mbProj ?. DBProjectId ==. mbHdw ?. DBHalfDayWorkedProjectId)
        on (just (hd ^. DBHalfDayId) ==. mbHdw ?. DBHalfDayWorkedHalfDayId)
        orderBy [asc (hd ^. DBHalfDayDay), desc (hd ^. DBHalfDayTimeInDay)]
        pure (hd, mbHdw, mbProj)
    mapM dbToHalfDayInt tupleList

-- | Check that the constraints on the times are valid between the two half-days
timesAreOrderedInDay ::
    TimeInDay ->
    DBHalfDayWorked ->
    Maybe DBHalfDayWorked ->
    Bool
timesAreOrderedInDay Morning hdw mbOtherHdw =
    isOrdered $ timesOfDay hdw ++ otherTimes
    where
        otherTimes = concatMap timesOfDay mbOtherHdw
-- We switch the arguments and call the same function
timesAreOrderedInDay Afternoon hdw (Just otherHdw) =
    timesAreOrderedInDay Morning otherHdw (Just hdw)
-- Afternoon only, just need to check for half day
timesAreOrderedInDay Afternoon hdw Nothing = isOrdered $ timesOfDay hdw

-- | Make sure that the times in hdw are ok:
--   - arrived < left
--   - consistent with the potential other hdw
guardNewTimesAreOk ::
    (MonadIO m, MonadUnliftIO m) =>
    Time.Day ->
    TimeInDay ->
    DBHalfDayWorked ->
    ExceptT (OpenUnion '[TimesAreWrong]) (SqlPersistT m) ()
guardNewTimesAreOk day tid hdw = do
    eiHdHdwProj <- lift . runExceptT $ hdHdwProjGetInt day $ other tid
    let mbOtherHdw = case eiHdHdwProj of
            Left (_ :: OpenUnion '[HdNotFound]) -> Nothing
            Right (_, Entity _ oHdw, _) -> Just oHdw
    -- Check if it works
    unless (timesAreOrderedInDay tid hdw mbOtherHdw) (throwIO TimesAreWrong)

-- | Return the times in the day in a list
timesOfDay :: DBHalfDayWorked -> [Time.TimeOfDay]
timesOfDay hdw = [dBHalfDayWorkedArrived hdw, dBHalfDayWorkedLeft hdw]

-- | Return true if the list is sorted
isOrdered :: (Ord a) => [a] -> Bool
isOrdered [] = True
isOrdered [_] = True
isOrdered (x : y : xs) = x <= y && isOrdered (y : xs)
