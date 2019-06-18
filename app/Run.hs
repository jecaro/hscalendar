module Run 
  ( App(..)
  , run
  , runDB
  )
where

import           RIO
import           RIO.Orphans()
import           RIO.Process (proc, runProcess)
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
import           Data.Text.IO (putStrLn) 
import qualified Formatting as F (int, left, sformat)
import           Formatting ((%.))
import           System.Environment (lookupEnv)


import           App 
    ( App(..)
    , HasConfig(..)
    , HasConnPool(..)
    , HasProcessContext(..)
    )
import           Config
    ( Config(..)
    , DefaultHours(..)
    , DefaultHoursForDay(..)
    )
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

import           Model
    ( HalfDay(..)
    , HalfDayWorked(..)
    , HdNotFound(..)
    , ProjExists(..)
    , ProjHasHDW(..)
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
    , migrateAll
    , projAdd
    , projectName 
    , projList
    , projRename
    , projRm
    , showDay
    )
import           TimeInDay (TimeInDay(..))

-- | The editor returns an error
newtype ProcessReturnsError = ProcessReturnsError String

instance Exception ProcessReturnsError

instance Show ProcessReturnsError where
    show (ProcessReturnsError cmd) = "The process " <> cmd <> " returns an error"

errProjCmdIsMandatory :: Text
errProjCmdIsMandatory = "There should be one project command"

-- | Print an exception 
printException :: (MonadIO m, Show a) => a -> m ()
printException e =  liftIO . putStrLn $ Text.pack $ show e

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
run :: (HasConnPool env, HasConfig env, HasLogFunc env, HasProcessContext env) 
    => Cmd 
    -> RIO env ()

-- Migrate the database
run Migrate = runDB $ runMigration migrateAll

-- List the projects
run ProjList = runDB projList >>= liftIO . mapM_ (putStrLn . projectName) 

-- Add a project
run (ProjAdd project) = catch (void $ runDB $ projAdd project) 
                           (\e@(ProjExists _) -> printException e)

-- Remove a project
run (ProjRm project) = catches (runDB $ projRm project) 
    [ Handler (\e@(ProjHasHDW _)   -> printException e)
    , Handler (\e@(ProjNotFound _) -> printException e)
    ]

-- Rename a project
run (ProjRename p1 p2) = catches (runDB $ projRename p1 p2)
    [ Handler (\e@(ProjExists _)   -> printException e)
    , Handler (\e@(ProjNotFound _) -> printException e)
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
           Right ((HalfDay _ _ hdt), Nothing) -> [ (Text.pack . show) hdt ]
           Right (_, Just (HalfDayWorked notes tArrived tLeft office _ _, project)) ->
               [ (Text.pack . show) office <> ":  " <> showTime tArrived <> " - " <> showTime tLeft
               , "Project: " <> projectName project
               , "Notes:   " <> notes
               ]
    -- Print it
    liftIO $ mapM_ putStrLn hdStr
  where showTime (Time.TimeOfDay h m _) = 
            Text.intercalate ":" $ fmap (F.sformat (F.left 2 '0' %. F.int)) [h, m]

-- Edit an entry
run (DiaryEdit _ _) = do
    -- Find an editor
    editor <- liftIO $ fromMaybe "vim" <$> lookupEnv "EDITOR"
    -- Launch process
    exitCode <- proc editor [] runProcess
    -- Handle error code
    when (exitCode /= ExitSuccess) (throwIO $ ProcessReturnsError editor)
    -- Parse result
    -- Delete tmp file
    -- Commit to database
    return ()

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
            config <- view configL
            -- Get the default times from the config file
            let (DefaultHours dArrived dLeft) = case tid of
                    Morning   -> morning (defaultHours config)
                    Afternoon -> afternoon (defaultHours config)
            -- Get the arrived and left commands if they exists, maintaining 
            -- the other options
                (mbArrived, otherOpts') = findArrivedCmd otherOpts
                (mbLeft, otherOpts'') = findLeftCmd otherOpts'
            -- Unwarp maybe and the newtype
                arrived = maybe dArrived (\(SetArrived a) -> a) mbArrived
                left    = maybe dLeft (\(SetLeft a) -> a) mbLeft
            -- Carry on, we have now everything to create the hwd
            eiAdded <- try $ runDB $ hdSetWork day tid proj (defaultOffice config) arrived left
            case eiAdded of
                Right _ -> return $ Right otherOpts''
                Left e@(ProjNotFound _) -> return $ Left $ Text.pack $ show e
        -- Holiday but no project
        (Right (_, Nothing), (Nothing, _)) -> return $ Left errProjCmdIsMandatory
        -- No hd, but no project either
        (Left (HdNotFound _ _), (Nothing, _)) -> return $ Left errProjCmdIsMandatory
    
    -- Apply remaining options
    case eiOtherOpts of
        Left msg -> liftIO $ putStrLn msg
        Right otherOpts -> do
            -- Apply set arrived set left when we have the two options
            let (mbAL, otherOpts') = findArrivedAndLeftCmd otherOpts
            case mbAL of
                Just (SetArrived a, SetLeft l) -> 
                     catch (runDB $ hdwSetArrivedAndLeft day tid a l) 
                         (\e@TimesAreWrong -> printException e)
                Nothing -> return ()
            -- Then apply remaining commands
            runDB $ mapM_ dispatchEditWithError otherOpts' 
            -- Display new Half-Day
            run $ DiaryDisplay cd tid
          where dispatchEditWithError x = catch (dispatchEdit day tid x) 
                    (\e@TimesAreWrong -> printException e)
 
-- Set a holiday entry
run (DiaryHoliday cd tid hdt) = do
    day <- toDay cd
    runDB $ hdSetHoliday day tid hdt
    -- Display new Half-Day
    run $ DiaryDisplay cd tid

-- Delete an entry
run (DiaryRm cs tid) = do
    day <- toDay cs
    catch (runDB $ hdRm day tid) (\e@(HdNotFound _ _) -> printException e)

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


-- | Run sql actions with the pool
runDB :: (HasConnPool env) => SqlPersistM a-> RIO env a
runDB actions = do
    pool <- view connPoolL
    liftIO $ runSqlPersistMPool actions pool

