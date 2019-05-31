import           RIO
import           RIO.Orphans()
import qualified RIO.Text as Text (pack)

import           Control.Monad.IO.Class (liftIO)
import           Database.Persist.Sqlite
    ( runMigration
    , withSqlitePool
    )
import           Data.Yaml (prettyPrintParseException)
import           Options.Applicative (execParser)
import           Path (toFilePath)

import           Config (Config(..), getConfig)
import           CommandLine 
    ( Options(..)
    , opts
    )
import           Model
import           Run (App(..), run, runDB)

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
                    -- Initialize the application
                    let app = App { appLogFunc  = lf
                                  , appConnPool = pool }

                    -- Run the app
                    liftIO $ runRIO app $ do
                        runDB $ runMigration migrateAll
                        run cmd
              where dbFile = Text.pack $ toFilePath $ db config
