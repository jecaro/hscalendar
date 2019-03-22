import           Control.Exception.Safe (MonadCatch, try, catch)
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (runNoLoggingT)
import           Database.Persist.Sqlite
    ( SqlPersistT
    , runMigration
    , runSqlPool
    , withSqlitePool
    )
import           Data.List (intercalate)
import           Data.Time.Calendar (Day)
import           Data.Time.LocalTime (TimeOfDay(..))
import           Options.Applicative (execParser)
import           Text.Printf (printf)

import           CommandLine 
    ( Cmd(..)
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
    , projList
    , projRename
    , projRm
    , showDay
    )

-- Synopsis
-- hsmaster diary work -d date -m|-a [commands]
--   commands: 
--     -p project   set the project name 
--     -n note      set note
--     -a 00:00     set arrived time
--     -l 00:00     set left time
-- hsmaster diary holiday date morning|afternoon
-- hsmaster diary rm date morning|afternoon
-- hsmaster diary display date morning|afternoon
-- hsmaster project list
-- hsmaster project rm project
-- hsmaster project add project
-- hsmaster project rename project1 project2 

-- TODO:
-- - Error handling in parser -> show message
-- - Add import CSV
-- - Add stats for a year
-- - Add optional day/time
-- - Make diary date/TimeInDay optional default today
-- - Append note
-- - Launch editor
-- - Project empty string
-- - Add unit testing
-- - Remove public holiday
-- - Put -p as positional parameter
-- - Use Lens instead of records
-- - Cascade delete
-- - Use unliftio instead of safe-exceptions
-- - Handle exception from optparse-applicative
-- - Add standard documentation
-- - Sort functions in CommandLine module
-- - Show day of the week
-- - Add colors/bold

-- Ideas
-- - put default values in a config file as well as open days
-- - put db file in a config file as well

errProjCmdIsMandatory :: String
errProjCmdIsMandatory = "There should be one project command"

-- Get out the first element of a list which return Just 
partitionFirst :: (a -> Maybe b) -> [a] -> (Maybe b, [a])
partitionFirst _ [] = (Nothing, [])
partitionFirst p (x:xs) =
    case p x of
        r@(Just _) -> (r, xs)
        Nothing    -> (r', x:xs')
          where (r', xs') = partitionFirst p xs

-- Find a SetProj command
findProjCmd :: [WorkOption] -> (Maybe SetProj, [WorkOption])
findProjCmd = partitionFirst getProj 
  where getProj (MkSetProj s@(SetProj _)) = Just s
        getProj _ = Nothing

-- Find a SetArrived command
findArrivedCmd :: [WorkOption] -> (Maybe SetArrived, [WorkOption])
findArrivedCmd = partitionFirst getArrived 
  where getArrived (MkSetArrived s@(SetArrived _)) = Just s
        getArrived _ = Nothing

-- Find a SetLeft command
findLeftCmd :: [WorkOption] -> (Maybe SetLeft, [WorkOption])
findLeftCmd = partitionFirst getLeft 
  where getLeft (MkSetLeft s@(SetLeft _)) = Just s
        getLeft _ = Nothing

-- Find both arrived and left command
findArrivedAndLeftCmd 
    :: [WorkOption] 
    -> (Maybe (SetArrived, SetLeft), [WorkOption])
findArrivedAndLeftCmd options = 
    let (mbArrived, options')  = findArrivedCmd options
        (mbLeft,    options'') = findLeftCmd options'
    in case (mbArrived, mbLeft) of
        (Just arrived, Just left) -> (Just (arrived, left), options'')
        _                         -> (Nothing, options)

-- List projects
run :: (MonadIO m, MonadCatch m) => Cmd -> SqlPersistT m ()
run ProjList = projList >>= liftIO . mapM_ (putStrLn . projectName)

-- Add a project
run (ProjAdd name) = catch (void $ projAdd $ Project name) 
                           (\(ModelException msg) -> liftIO . putStrLn $ msg)

-- Remove a project
-- TODO ask for confirmation when erasing hdw
run (ProjRm name) = catch (projRm $ Project name) 
                          (\(ModelException msg) -> liftIO . putStrLn $ msg)

run (ProjRename name1 name2) = catch 
    (projRename p1 p2) (\(ModelException msg) -> liftIO . putStrLn $ msg)
  where p1 = Project name1
        p2 = Project name2

-- Display an entry
run (DiaryDisplay cd tid) = do
    -- Get actual day
    day <- toDay cd
    -- Display input date
    liftIO . putStrLn $ showDay day ++ " " ++ show tid
    -- Get half-day
    eiHdHdwProj <- try $ hdHdwProjGet day tid
    -- Analyse output to produce lines of text
    let hdStr = case eiHdHdwProj of
           Left (ModelException msg) -> [ msg ]
           Right (_, Nothing)        -> [ show Holiday ]
           Right (_, Just (HalfDayWorked notes arrived left office _ _, Project name)) ->
               [ show office ++ ":  " ++ showTime arrived ++ " - " ++ showTime left
               , "Project: " ++ name
               , "Notes:   " ++ notes
               ]
    -- Print it
    liftIO $ mapM_ putStrLn hdStr
  where showTime (TimeOfDay h m _) = 
            intercalate ":" $ fmap (printf "%02d") [h, m]

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
            eiAdded <- try $ hdSetWork day tid $ Project proj
            case eiAdded of
                Right _ -> return $ Right otherOpts
                Left (ModelException msg) -> return $ Left msg
        -- Holiday but no project
        (Right (_, Nothing), (Nothing, _)) -> return $ Left errProjCmdIsMandatory
        -- No hd, but no project either
        (Left (ModelException _), (Nothing, _)) -> return $ Left errProjCmdIsMandatory
    
    -- Apply remaining options
    case eiOtherOpts of
        Left msg -> liftIO $ putStrLn msg
        Right otherOpts -> do
            -- Apply set arrived set left when we have to two options
            let (mbAL, otherOpts') = findArrivedAndLeftCmd otherOpts
            case mbAL of
                Just (SetArrived a, SetLeft l) -> 
                    catch (hdwSetArrivedAndLeft day tid a l) 
                        (\(ModelException msg) -> liftIO $ putStrLn msg)
                Nothing -> return ()
            -- Then apply remaining commands
            mapM_ dispatchEditWithError otherOpts' 
            -- Display new Half-Day
            run $ DiaryDisplay cd tid
          where dispatchEditWithError x = 
                    catch (dispatchEdit day tid x) 
                          (\(ModelException msg) -> liftIO $ putStrLn msg)

-- Set a holiday entry
run (DiaryHoliday cd tid) = do
    day <- toDay cd
    hdSetHoliday day tid
    -- Display new Half-Day
    run $ DiaryDisplay cd tid

-- Delete an entry
run (DiaryRm cs tid) = do
    day <- toDay cs
    catch (hdRm day tid) (\(ModelException msg) -> liftIO $ putStrLn msg)

-- Dispatch edit
dispatchEdit
    :: (MonadIO m, MonadCatch m)
    => Day
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
dispatchEdit day tid (MkSetProj (SetProj name))       = hdwSetProject day tid $ Project name

main :: IO ()
-- runNoLoggingT or runStdoutLoggingT
main = runNoLoggingT . withSqlitePool "file.db" 3 . runSqlPool $ do
    runMigration migrateAll
    liftIO (execParser opts) >>= run

