module Run 
  ( App(..)
  , run
  , runDB
  )
where

import           RIO
import           RIO.Orphans()
import qualified RIO.Text as Text (intercalate, pack)
import qualified RIO.Time as Time (Day, TimeOfDay(..))

import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Database.Persist.Sqlite
    ( SqlPersistT
    , SqlPersistM
    , runMigration
    , runSqlPersistMPool
    )
import           Database.Persist.Sql (ConnectionPool)
import           Data.Text.IO (putStrLn) 
import qualified Formatting as F (int, left, sformat)
import           Formatting ((%.))

import           Config(Config, defaultHours)
import           CommandLine 
    ( Cmd(..)
    , SetArrived(..)
    , SetLeft(..)
    , SetNotes(..)
    , SetOffice(..)
    , SetProj(..)
    , WorkOption(..)
    )
import           CustomDay(toDay)

import           DefaultHours(DefaultHours(..), morning, afternoon)
import           HalfDayType (HalfDayType(..))
import           Model
import           ModelFcts
    ( HdNotFound(..)
    , ProjExists(..)
    , ProjNotFound(..)
    , TimesAreWrong(..)
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
    , projList
    , projRename
    , projRm
    , showDay
    )
import           TimeInDay (TimeInDay(..))

errProjCmdIsMandatory :: Text
errProjCmdIsMandatory = "There should be one project command"

-- | Get out the first element of a list which return Just 
partitionFirst :: (a -> Maybe b) -> [a] -> (Maybe b, [a])
partitionFirst _ [] = (Nothing, [])
partitionFirst p (x:xs) =
    case p x of
        r@(Just _) -> (r, xs)
        Nothing    -> (r', x:xs')
          where (r', xs') = partitionFirst p xs

-- | Find a SetProj command
findProjCmd :: [WorkOption] -> (Maybe SetProj, [WorkOption])
findProjCmd = partitionFirst getProj 
  where getProj (MkSetProj s@(SetProj _)) = Just s
        getProj _ = Nothing

-- | Find a SetArrived command
findArrivedCmd :: [WorkOption] -> (Maybe SetArrived, [WorkOption])
findArrivedCmd = partitionFirst getArrived 
  where getArrived (MkSetArrived s@(SetArrived _)) = Just s
        getArrived _ = Nothing

-- | Find a SetLeft command
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

-- | Execute the command
run :: (HasConnPool env, HasConfig env) => Cmd -> RIO env ()

-- Migrate the database
run Migrate = runDB $ runMigration migrateAll

-- List the projects
run ProjList = runDB projList >>= liftIO . mapM_ (putStrLn . projectName) 

-- Add a project
run (ProjAdd project) = catch (void $ runDB $ projAdd project) 
                           (\e@(ProjExists _) -> liftIO . putStrLn $ Text.pack $ show e)

-- Remove a project
run (ProjRm project) = catch (runDB $ projRm project) 
                           (\e@(ProjExists _) -> liftIO . putStrLn $ Text.pack $ show e)

-- Rename a project
run (ProjRename p1 p2) = catches (runDB $ projRename p1 p2)
    [ Handler (\e@(ProjExists _)   -> liftIO . putStrLn $ Text.pack $ show e)
    , Handler (\e@(ProjNotFound _) -> liftIO . putStrLn $ Text.pack $ show e)
    ]

-- Display an entry
run (DiaryDisplay cd tid) = do
    -- Get actual day
    day <- toDay cd
    -- Display input date
    liftIO . putStrLn $ showDay day <> " " <> (Text.pack . show) tid
    -- Get half-day
    eiHdHdwProj <- try $ runDB $ hdHdwProjGet day tid
    -- Analyse output to produce lines of text
    let hdStr = case eiHdHdwProj of
           Left e@(HdNotFound _ _) -> [ (Text.pack . show) e ]
           Right (_, Nothing) -> [ (Text.pack . show) Holiday ]
           Right (_, Just (HalfDayWorked notes tArrived tLeft office _ _, project)) ->
               [ (Text.pack . show) office <> ":  " <> showTime tArrived <> " - " <> showTime tLeft
               , "Project: " <> projectName project
               , "Notes:   " <> notes
               ]
    -- Print it
    liftIO $ mapM_ putStrLn hdStr
  where showTime (Time.TimeOfDay h m _) = 
            Text.intercalate ":" $ fmap (F.sformat (F.left 2 '0' %. F.int)) [h, m]

-- Set a work entry 
run (DiaryWork cd tid wopts) = do

    -- Get actual day
    day <- toDay cd

    -- Get hdw
    eiHdHdwProj <- try $ runDB $ hdHdwProjGet day tid
 
    -- Create it with a project if needed
    eiOtherOpts <- case (eiHdHdwProj, findProjCmd wopts) of
        -- Everything is there
        (Right (_, Just (_, _)), _) -> return $ Right wopts 
        -- Nothing or holiday
        (_, (Just (SetProj proj), otherOpts)) -> do
            defaultHoursForDay <- fmap defaultHours $ view configL
            let (DefaultHours arrived left) = case tid of
                    Morning   -> morning defaultHoursForDay
                    Afternoon -> afternoon defaultHoursForDay
            eiAdded <- try $ runDB $ hdSetWork day tid proj arrived left
            case eiAdded of
                Right _ -> return $ Right otherOpts
                Left e@(ProjNotFound _) -> return $ Left $ Text.pack $ show e
        -- Holiday but no project
        (Right (_, Nothing), (Nothing, _)) -> return $ Left errProjCmdIsMandatory
        -- No hd, but no project either
        (Left (HdNotFound _ _), (Nothing, _)) -> return $ Left errProjCmdIsMandatory
    
    -- Apply remaining options
    case eiOtherOpts of
        Left msg -> liftIO $ putStrLn msg
        Right otherOpts -> do
            -- Apply set arrived set left when we have to two options
            let (mbAL, otherOpts') = findArrivedAndLeftCmd otherOpts
            case mbAL of
                Just (SetArrived a, SetLeft l) -> 
                     catch (runDB $ hdwSetArrivedAndLeft day tid a l) 
                         (\e@TimesAreWrong -> liftIO $ putStrLn $ Text.pack $ show e)
                Nothing -> return ()
            -- Then apply remaining commands
            runDB $ mapM_ dispatchEditWithError otherOpts' 
            -- Display new Half-Day
            run $ DiaryDisplay cd tid
          where dispatchEditWithError x = catch (dispatchEdit day tid x) 
                    (\e@TimesAreWrong -> liftIO $ putStrLn $ Text.pack $ show e)
 
-- Set a holiday entry
run (DiaryHoliday cd tid) = do
    day <- toDay cd
    runDB $ hdSetHoliday day tid
    -- Display new Half-Day
    run $ DiaryDisplay cd tid

-- Delete an entry
run (DiaryRm cs tid) = do
    day <- toDay cs
    catch (runDB $ hdRm day tid) (\e@(HdNotFound _ _) -> liftIO $ putStrLn $ Text.pack $ show e)

-- Dispatch edit
dispatchEdit
    :: (MonadUnliftIO m)
    => Time.Day
    -> TimeInDay
    -> WorkOption
    -> SqlPersistT m()
-- Set arrived time
dispatchEdit day tid (MkSetArrived (SetArrived time)) = hdwSetArrived day tid time
-- Set left time
dispatchEdit day tid (MkSetLeft (SetLeft time))       = hdwSetLeft day tid time
-- Simple actions handling
dispatchEdit day tid (MkSetNotes (SetNotes notes))    = hdwSetNotes day tid notes
dispatchEdit day tid (MkSetOffice (SetOffice office)) = hdwSetOffice day tid office
dispatchEdit day tid (MkSetProj (SetProj project))    = hdwSetProject day tid project

-- | The app data
data App = App
    { appLogFunc  :: !LogFunc        -- ^ The log function
    , appConnPool :: !ConnectionPool -- ^ The connexion pool
    , appConfig   :: !Config         -- ^ The configuration file
    }

class HasConnPool env where
    connPoolL :: Lens' env ConnectionPool

instance HasConnPool App where
    connPoolL = lens appConnPool (\x y -> x { appConnPool = y })

instance HasLogFunc App where
    logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

class HasConfig env where
    configL :: Lens' env Config

instance HasConfig App where
    configL = lens appConfig (\x y -> x { appConfig = y })

-- | Run sql actions with the pool
runDB :: (HasConnPool env) => SqlPersistM a-> RIO env a
runDB actions = do
    pool <- view connPoolL
    liftIO $ runSqlPersistMPool actions pool

