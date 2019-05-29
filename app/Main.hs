{-# LANGUAGE LiberalTypeSynonyms #-}

import           RIO
import           RIO.Orphans()
import qualified RIO.Text as Text (intercalate, pack)
import qualified RIO.Time as Time (Day, TimeOfDay(..))

import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Database.Persist.Sqlite
    ( SqlPersistT
    , SqlPersistM
    , runMigration
    , runSqlPersistMPool
    , withSqlitePool
    )
import           Data.Text.IO (putStrLn) 
import           Data.Yaml (prettyPrintParseException)
import qualified Formatting as F (int, left, sformat)
import           Formatting ((%.))
import           Options.Applicative (execParser)
import           Path (toFilePath)

import           Config (Config(..), getConfig)
import           CommandLine 
    ( Cmd(..)
    , Options(..)
    , SetArrived(..)
    , SetLeft(..)
    , SetNotes(..)
    , SetOffice(..)
    , SetProj(..)
    , WorkOption(..)
    , opts
    )
import           CustomDay(toDay)

import           HalfDayType (HalfDayType(..))
import           Model
import           TimeInDay (TimeInDay(..))
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

-- Synopsis
-- hsmaster diary work date -m|-a [commands]
--   commands: 
--     -p project   set the project name 
--     -n note      set note
--     -a 00:00     set arrived time
--     -l 00:00     set left time
-- hsmaster diary holiday date -m|-a
-- hsmaster diary rm date -m|-a
-- hsmaster diary display date -m|-a
-- hsmaster project list
-- hsmaster project rm project
-- hsmaster project add project
-- hsmaster project rename project1 project2 

-- TODO:
-- - Add init function to create the database
-- - Add import CSV
-- - Add stats for a year
-- - Add optional day/time
-- - Make diary date/TimeInDay optional default today
-- - Append note
-- - Launch editor
-- - Remove public holiday
-- - Put -p as positional parameter
-- - Use Lens instead of records
-- - Forbid to delete a project if there are links on it
-- - Add colors/bold
-- - Use esqueleto for join (project, hdw)
-- - put default values in a config file as well as open days
-- - put db file in a config file as well
-- - replace pattern matching with record syntax

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
run :: (MonadIO m, MonadUnliftIO m) => Cmd -> SqlPersistT m ()

-- List the projects
run ProjList = projList >>= liftIO . mapM_ (putStrLn . projectName)

-- Add a project
run (ProjAdd project) = catch (void $ projAdd project) 
                           (\e@(ProjExists _) -> liftIO . putStrLn $ Text.pack $ show e)

-- Remove a project
run (ProjRm project) = catch (projRm project) 
                           (\e@(ProjExists _) -> liftIO . putStrLn $ Text.pack $ show e)
-- Rename a project
run (ProjRename p1 p2) = catches (projRename p1 p2)
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
    eiHdHdwProj <- try $ hdHdwProjGet day tid
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
    eiHdHdwProj <- try $ hdHdwProjGet day tid
 
    -- Create it with a project if needed
    eiOtherOpts <- case (eiHdHdwProj, findProjCmd wopts) of
        -- Everything is there
        (Right (_, Just (_, _)), _) -> return $ Right wopts 
        -- Nothing or holiday
        (_, (Just (SetProj proj), otherOpts)) -> do
            eiAdded <- try $ hdSetWork day tid proj
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
                    catch (hdwSetArrivedAndLeft day tid a l) 
                        (\e@TimesAreWrong -> liftIO $ putStrLn $ Text.pack $ show e)
                Nothing -> return ()
            -- Then apply remaining commands
            mapM_ dispatchEditWithError otherOpts' 
            -- Display new Half-Day
            run $ DiaryDisplay cd tid
          where dispatchEditWithError x = catch (dispatchEdit day tid x) 
                    (\e@TimesAreWrong -> liftIO $ putStrLn $ Text.pack $ show e)

-- Set a holiday entry
run (DiaryHoliday cd tid) = do
    day <- toDay cd
    hdSetHoliday day tid
    -- Display new Half-Day
    run $ DiaryDisplay cd tid

-- Delete an entry
run (DiaryRm cs tid) = do
    day <- toDay cs
    catch (hdRm day tid) (\e@(HdNotFound _ _) -> liftIO $ putStrLn $ Text.pack $ show e)

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

type RunDB = forall a. SqlPersistM a -> IO a

data App = App
    { appLogFunc :: !LogFunc,
      appSQLFunc :: !RunDB
    }

class HasSQLFunc env where
    getSQLFunc :: env -> RunDB

instance HasSQLFunc App where
    getSQLFunc = appSQLFunc

runDB :: (HasSQLFunc env) => SqlPersistM a-> RIO env a
runDB actions = do
    sqlF <- asks getSQLFunc
    liftIO $ sqlF actions

-- | Main function
main :: IO ()
main = do
    -- Parse command line
    (Options verbose level, cmd) <- execParser opts
    -- Init log options: level and verbose mode
    logOptions <- setLogMinLevel level <$> logOptionsHandle stderr verbose 
    -- Read config file with logger
    withLogFunc logOptions $ \lf -> try (runRIO lf getConfig) >>= 
        \case 
            -- Error with the config file end of the program
            Left e -> runRIO lf $ logError $ display $ Text.pack (prettyPrintParseException e)
            -- Got config file carry on
            Right config ->
                -- Create the sql pool with RIO to handle log
                runRIO lf $ withSqlitePool dbFile 3 $ \pool -> do
                    -- Initialise the application
                    let app = App { appLogFunc = lf
                                  , appSQLFunc = \actions -> 
                                        runSqlPersistMPool actions pool }

                    -- Run the app
                    liftIO $ runRIO app $
                        runDB $ do 
                            runMigration migrateAll
                            run cmd
              where dbFile = Text.pack $ toFilePath $ db config
