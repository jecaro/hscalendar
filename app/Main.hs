import           RIO
import           RIO.Orphans()
import           RIO.Process (mkDefaultProcessContext)
import qualified RIO.Text as Text (pack)

import           Control.Monad.IO.Class (liftIO)
import           Database.Persist.Sqlite
    ( withSqlitePool
    )
import           Data.Yaml (prettyPrintParseException)
import           Options.Applicative (execParser)
import           Path (toFilePath)
import           System.Exit (exitFailure)

import           Config (Config(..), getConfig)
import           CommandLine 
    ( Options(..)
    , opts
    )
import           Run (App(..), run)

-- Synopsis
-- hsmaster diary work date -m|-a [commands]
--   commands: 
--     -p project   set the project name 
--     -n note      set note
--     -a 00:00     set arrived time
--     -l 00:00     set left time
-- hsmaster diary holiday date -m|-a [cp|ef|rtte|rtts|ss|ph|tp]
-- hsmaster diary rm date -m|-a
-- hsmaster diary display date -m|-a
-- hsmaster diary edit date -m|-a
-- hsmaster project list
-- hsmaster project rm project
-- hsmaster project add project
-- hsmaster project rename project1 project2 

-- TODO:
-- - Add import CSV
-- - Add stats for a year
-- - Add show week
-- - Add show month
-- - Make diary date/TimeInDay optional default today
-- - Put defaults values in config file for a week
-- - Add unit test for editor features
-- - Add cleanUp command
-- - Comments modules
-- - Rename the Model functions (remove hdw)

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
            Left e -> runRIO lf $ do
                logError $ display $ Text.pack (prettyPrintParseException e)
                liftIO exitFailure
            -- Got config file carry on
            Right config ->
                -- Create the sql pool with RIO to handle log
                runRIO lf $ withSqlitePool dbFile 3 $ \pool -> do
                    pc <- mkDefaultProcessContext
                    -- Initialize the application
                    let app = App { appLogFunc        = lf
                                  , appConnPool       = pool 
                                  , appConfig         = config
                                  , appProcessContext = pc }

                    -- Run the app, handle exceptions (should be only because
                    -- the database is not initialized)
                    liftIO $ runRIO app $ catch (run cmd) (\e -> do
                        logError ("Error: " <> display (e :: SomeException))
                        liftIO exitFailure)
              where dbFile = Text.pack $ toFilePath $ db config
