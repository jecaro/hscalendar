-- | Application global read only state
module App.App
    ( App(..)
    , HasConnPool(..)
    , HasConfig(..)
    , HasProcessContext(..)
    , initAppAndRun
    )
where

import           RIO
import           RIO.Orphans()
import           RIO.Process (HasProcessContext(..), ProcessContext, mkDefaultProcessContext)
import           RIO.Text as Text (pack)

import           Database.Persist.Sqlite (withSqlitePool)
import           Database.Persist.Sql (ConnectionPool)
import           Data.Yaml (prettyPrintParseException)
import           System.Exit (exitFailure)
import           Path (toFilePath)

import           App.Config(Config(..), getConfig)

-- | The app data
data App = App
    { appLogFunc        :: !LogFunc        -- ^ The log function
    , appConnPool       :: !ConnectionPool -- ^ The connexion pool
    , appConfig         :: !Config         -- ^ The configuration file
    , appProcessContext :: !ProcessContext -- ^ Context to start processes
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

instance HasProcessContext App where
    processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })


initAppAndRun :: MonadUnliftIO m => Bool -> LogLevel -> RIO App b -> m b
initAppAndRun verbose level actions = do
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

                    -- Run the app, handle exceptions 
                    liftIO $ runRIO app $ catch actions (\e -> do
                        logError ("Error: " <> display (e :: SomeException))
                        liftIO exitFailure)
              where dbFile = Text.pack $ toFilePath $ db config

