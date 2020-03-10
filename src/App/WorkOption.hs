-- | Set of options operating on a working half-day
module App.WorkOption
    ( SetArrived(..)
    , SetLeft(..)
    , SetNotes(..)
    , SetOffice(..)
    , SetProj(..)
    , runWorkOptions
    , ProjCmdIsMandatory(..)
    , WorkOption(..)
    )
where

import           RIO
import qualified RIO.Time as Time

import           Data.Aeson (FromJSON, ToJSON)
import           Database.Persist.Sql (SqlPersistT)

import           App.App
    ( HasConfig(..)
    , HasConnPool(..)
    , runDB
    )
import           App.Config
    ( afternoon
    , defaultHours
    , DefaultHours(..)
    , defaultOffice
    , morning
    )

import           Db.HalfDay (HalfDay(..))
import           Db.Model
    ( HdNotFound(..)
    , hdGet
    , hdSetArrived
    , hdSetLeft
    , hdSetNotes
    , hdSetOffice
    , hdSetProject
    , hdSetArrivedAndLeft
    , hdSetWork
    )
import           Db.Notes (Notes(..))
import           Db.Office (Office(..))
import           Db.Project (Project(..))
import           Db.TimeInDay (TimeInDay(..))

-- | When setting a work half-day, a project command is mandatory
data ProjCmdIsMandatory = ProjCmdIsMandatory

instance Exception ProjCmdIsMandatory

instance Show ProjCmdIsMandatory where
    show ProjCmdIsMandatory = "There should be one project command"

-- | Command for setting the project
newtype SetProj = SetProj Project
    deriving (Eq, Generic, Show)

instance FromJSON SetProj
instance ToJSON SetProj

-- | Command for setting the notes
newtype SetNotes = SetNotes Notes
    deriving (Eq, Generic, Show)

instance FromJSON SetNotes
instance ToJSON SetNotes

-- | Command for changing the time of arrival
newtype SetArrived = SetArrived Time.TimeOfDay
    deriving (Eq, Generic, Show)

instance FromJSON SetArrived
instance ToJSON SetArrived

-- | Command for changing the departure time
newtype SetLeft = SetLeft Time.TimeOfDay
    deriving (Eq, Generic, Show)

instance FromJSON SetLeft
instance ToJSON SetLeft

-- | Command for setting the office
newtype SetOffice = SetOffice Office
    deriving (Eq, Generic, Show)

instance FromJSON SetOffice
instance ToJSON SetOffice

-- | Sum type to contains all these commands
data WorkOption = MkSetArrived SetArrived |
                  MkSetLeft SetLeft       |
                  MkSetNotes SetNotes     |
                  MkSetOffice SetOffice   |
                  MkSetProj SetProj
    deriving (Eq, Generic, Show)

instance FromJSON WorkOption
instance ToJSON WorkOption

-- | Get out the first element of a list which return 'Just'
partitionFirst :: (a -> Maybe b) -> [a] -> (Maybe b, [a])
partitionFirst _ [] = (Nothing, [])
partitionFirst p (x:xs) =
    case p x of
        r@(Just _) -> (r, xs)
        Nothing    -> (r', x:xs')
          where (r', xs') = partitionFirst p xs

-- | Find a 'SetProj' command
findProjCmd :: [WorkOption] -> (Maybe SetProj, [WorkOption])
findProjCmd = partitionFirst getProj
  where getProj (MkSetProj s@(SetProj _)) = Just s
        getProj _ = Nothing

-- | Find a 'SetArrived' command
findArrivedCmd :: [WorkOption] -> (Maybe SetArrived, [WorkOption])
findArrivedCmd = partitionFirst getArrived
  where getArrived (MkSetArrived s@(SetArrived _)) = Just s
        getArrived _ = Nothing

-- | Find a 'SetLeft' command
findLeftCmd :: [WorkOption] -> (Maybe SetLeft, [WorkOption])
findLeftCmd = partitionFirst getLeft
  where getLeft (MkSetLeft s@(SetLeft _)) = Just s
        getLeft _ = Nothing

-- | Find both arrived and left command
findArrivedAndLeftCmd
    :: [WorkOption]
    -> (Maybe (SetArrived, SetLeft), [WorkOption])
findArrivedAndLeftCmd options =
    let (mbArrived, options')  = findArrivedCmd options
        (mbLeft,    options'') = findLeftCmd options'
    in case (mbArrived, mbLeft) of
        (Just tArrived, Just tLeft) -> (Just (tArrived, tLeft), options'')
        _                           -> (Nothing, options)

-- | Dispatch edit
dispatchEdit
    :: (MonadUnliftIO m)
    => Time.Day
    -> TimeInDay
    -> WorkOption
    -> SqlPersistT m()
-- Set arrived time
dispatchEdit day tid (MkSetArrived (SetArrived time)) = hdSetArrived day tid time
-- Set left time
dispatchEdit day tid (MkSetLeft (SetLeft time))       = hdSetLeft day tid time
-- Simple actions handling
dispatchEdit day tid (MkSetNotes (SetNotes notes))    = hdSetNotes day tid notes
dispatchEdit day tid (MkSetOffice (SetOffice office)) = hdSetOffice day tid office
dispatchEdit day tid (MkSetProj (SetProj project))    = hdSetProject day tid project

-- | Execute the work options. In order to do that, the fct checks if the record
--   is already a work half-day. If not it searches the mandatory project
--   command to be able to create it. It uses for that arrived and left times set
--   in the config file or on the command line. Then it applies the remaining
--   options. Error is handled with exceptions by the "Db.Model" module and catch
--   by the caller.
runWorkOptions :: (HasConnPool env, HasConfig env)
    => Time.Day -> TimeInDay -> [WorkOption] -> RIO env ()
runWorkOptions day tid wopts = do
    -- Get hdw
    eiHd <- try $ runDB $ hdGet day tid

    -- Create it with a project if needed
    otherOpts <- case (eiHd, findProjCmd wopts) of
        -- Everything is there
        (Right (MkHalfDayWorked _), _) -> pure wopts
        -- Nothing or holiday but a project
        (_, (Just (SetProj proj), otherOpts)) -> do
            config <- view configL
            -- Get the default times from the config file
            let (DefaultHours dArrived dLeft) = case tid of
                    Morning   -> config ^. defaultHours . morning
                    Afternoon -> config ^. defaultHours . afternoon
            -- Get the arrived and left commands if they exists, maintaining
            -- the other options
                (mbArrived, otherOpts') = findArrivedCmd otherOpts
                (mbLeft, otherOpts'') = findLeftCmd otherOpts'
            -- Unwarp maybe and the newtype
                arrived = maybe dArrived (\(SetArrived a) -> a) mbArrived
                left    = maybe dLeft (\(SetLeft a) -> a) mbLeft
            -- Carry on, we have now everything to create the hwd
            runDB $ hdSetWork day tid proj (config ^. defaultOffice) arrived left
            pure otherOpts''
        -- Holiday but no project
        (Right (MkHalfDayIdle _), (Nothing, _)) -> throwIO ProjCmdIsMandatory
        -- No hd, but no project either
        (Left (HdNotFound _ _), (Nothing, _)) -> throwIO ProjCmdIsMandatory

    -- Apply set arrived set left when we have the two options
    let (mbAL, otherOpts') = findArrivedAndLeftCmd otherOpts
    case mbAL of
        Just (SetArrived a, SetLeft l) -> runDB $ hdSetArrivedAndLeft day tid a l
        Nothing -> pure ()
    -- Then apply remaining commands
    runDB $ mapM_ (dispatchEdit day tid) otherOpts'
