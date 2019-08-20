module Run 
  ( App(..)
  , run
  , runDB
  )
where

import           RIO
import           RIO.Orphans()
import           RIO.Process (proc, runProcess)
import qualified RIO.Text as Text (pack)
import qualified RIO.Time as Time (Day)

import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Database.Persist.Sqlite
    ( SqlPersistT
    , SqlPersistM
    , runMigration
    , runSqlPersistMPool
    )
import           Data.Text.IO (putStrLn, readFile) 
import           System.Directory (removeFile)
import           System.Environment (lookupEnv)
import           System.IO.Temp (emptySystemTempFile)


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
import           Editor(ParseError(..), hdAsText, parse)
import           HalfDay (HalfDay(..))
import           Idle (Idle(..))
import           Model
    ( HdNotFound(..)
    , ProjExists(..)
    , ProjHasHd(..)
    , ProjNotFound(..)
    , TimesAreWrong(..)
    , hdGet
    , hdRm
    , hdSetHoliday
    , hdSetWork
    , hdSetArrived
    , hdSetArrivedAndLeft
    , hdSetLeft
    , hdSetNotes
    , hdSetOffice
    , hdSetProject
    , migrateAll
    , projAdd
    , projList
    , projRename
    , projRm
    , showDay
    , showTime
    )
import           Notes (unNotes)
import           Project (unProject)
import           TimeInDay (TimeInDay(..))
import           Worked (Worked(..))

-- | The editor returns an error
newtype ProcessReturnsError = ProcessReturnsError String

instance Exception ProcessReturnsError

instance Show ProcessReturnsError where
    show (ProcessReturnsError cmd) = "The process " <> cmd <> " returns an error"

data ProjCmdIsMandatory = ProjCmdIsMandatory

instance Exception ProjCmdIsMandatory

instance Show ProjCmdIsMandatory where
    show ProjCmdIsMandatory = "There should be one project command"

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

-- | Execute the work options. For this the fct checks if the record is already
--   a work half-day. If not it searches the mandatory project command to be
--   able to create it. It uses for so arrived and left time set in the config 
--   file or on the command line. Then it applies the remaining options. Error
--   is handled with exceptions by the Model module.
runWorkOptions :: (HasConnPool env, HasConfig env) 
    => Time.Day -> TimeInDay -> [WorkOption] -> RIO env ()
runWorkOptions day tid wopts = do
    -- Get hdw
    eiHd <- try $ runDB $ hdGet day tid
 
    -- Create it with a project if needed
    otherOpts <- case (eiHd, findProjCmd wopts) of
        -- Everything is there
        (Right (MkHalfDayWorked _), _) -> return wopts 
        -- Nothing or holiday but a project
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
            runDB $ hdSetWork day tid proj (defaultOffice config) arrived left
            return otherOpts''
        -- Holiday but no project
        (Right (MkHalfDayIdle _), (Nothing, _)) -> throwIO ProjCmdIsMandatory
        -- No hd, but no project either
        (Left (HdNotFound _ _), (Nothing, _)) -> throwIO ProjCmdIsMandatory

    -- Apply set arrived set left when we have the two options
    let (mbAL, otherOpts') = findArrivedAndLeftCmd otherOpts
    case mbAL of
        Just (SetArrived a, SetLeft l) -> runDB $ hdSetArrivedAndLeft day tid a l
        Nothing -> return ()
    -- Then apply remaining commands
    runDB $ mapM_ (dispatchEdit day tid) otherOpts' 

-- | Execute the command
run :: (HasConnPool env, HasConfig env, HasLogFunc env, HasProcessContext env) 
    => Cmd 
    -> RIO env ()

-- Migrate the database
run Migrate = runDB $ runMigration migrateAll

-- List the projects
run ProjList = runDB projList >>= liftIO . mapM_ (putStrLn . unProject) 

-- Add a project
run (ProjAdd project) = catch (void $ runDB $ projAdd project) 
                           (\e@(ProjExists _) -> printException e)

-- Remove a project
run (ProjRm project) = catches (runDB $ projRm project) 
    [ Handler (\e@(ProjHasHd _)    -> printException e)
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
    eiHd <- try $ runDB $ hdGet day tid
    -- Analyse output to produce lines of text
    let hdStr = case eiHd of
           Left e@(HdNotFound _ _) -> [ (Text.pack . show) e ]
           Right (MkHalfDayIdle (MkIdle _ _ hdt)) -> [ (Text.pack . show) hdt ]
           Right (MkHalfDayWorked (MkWorked _ _ tArrived tLeft office notes project)) ->
               [ (Text.pack . show) office <> ":  " <> showTime tArrived <> " - " <> showTime tLeft
               , "Project: " <> unProject project
               , "Notes:   " <> unNotes notes
               ]
    -- Print it
    liftIO $ mapM_ putStrLn hdStr

-- Edit an entry
run (DiaryEdit cd tid) = do
    -- Get the old record to put as default in the file
    day <- toDay cd
    oldRecord <- hdAsText <$> try (runDB $ hdGet day tid)
    -- Bracket to make sure the temporary file will be deleted no matter what
    fileContent <- bracket (liftIO $ emptySystemTempFile "hscalendar") 
        (liftIO . removeFile) 
        (\filename -> do
            -- Write old record
            writeFileUtf8 filename oldRecord
            -- Find an editor
            editor <- liftIO $ fromMaybe "vim" <$> lookupEnv "EDITOR"
            -- Launch process
            exitCode <- proc editor [filename] runProcess
            -- Handle error code
            when (exitCode /= ExitSuccess) (throwIO $ ProcessReturnsError editor)
            -- Read file content
            liftIO $ readFile filename
        )
    if fileContent == oldRecord 
        then nothingToDo
        else case parse fileContent of
            Left e@(ParserError _) -> throwIO e
            Left EmptyFileError    -> nothingToDo
            Right options          -> run $ DiaryWork cd tid options
  where nothingToDo = liftIO $ putStrLn "Nothing to do"

-- Set a work entry 
run (DiaryWork cd tid wopts) = do
    -- Get actual day
    day <- toDay cd
    -- Create the record in DB
    catches (runWorkOptions day tid wopts)
        [ Handler (\e@TimesAreWrong      -> printException e)
        , Handler (\e@ProjCmdIsMandatory -> printException e)
        ]
    -- Display new Half-Day
    run $ DiaryDisplay cd tid
 
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
dispatchEdit day tid (MkSetArrived (SetArrived time)) = hdSetArrived day tid time
-- Set left time
dispatchEdit day tid (MkSetLeft (SetLeft time))       = hdSetLeft day tid time
-- Simple actions handling
dispatchEdit day tid (MkSetNotes (SetNotes notes))    = hdSetNotes day tid notes
dispatchEdit day tid (MkSetOffice (SetOffice office)) = hdSetOffice day tid office
dispatchEdit day tid (MkSetProj (SetProj project))    = hdSetProject day tid project


-- | Run sql actions with the pool
runDB :: (HasConnPool env) => SqlPersistM a-> RIO env a
runDB actions = do
    pool <- view connPoolL
    liftIO $ runSqlPersistMPool actions pool

