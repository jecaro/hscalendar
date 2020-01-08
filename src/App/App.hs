-- | Application global read only state. This is used by the server and the
-- command line tool
module App.App
    ( App(..)
    , HasConnPool(..)
    , HasConfig(..)
    , logException
    , initAppAndRun
    , runDB
    )
where

import           RIO
import           RIO.Orphans()
import           RIO.Process (HasProcessContext(..), ProcessContext, mkDefaultProcessContext)
import           RIO.Text as Text (pack)

import           Database.Persist.Sqlite (withSqlitePool)
import           Database.Persist.Postgresql (withPostgresqlPool)
import           Database.Persist.Sql (ConnectionPool, SqlPersistM, runSqlPersistMPool)
import           Data.Yaml (prettyPrintParseException)
import           System.Exit (exitFailure)

import           App.Config
    ( backend
    , Config(..)
    , connectionString
    , DbBackend(..)
    , dbConfig
    , getConfig
    , nbConnections
    )

-- | The app data
data App = App
    { appLogFunc        :: !LogFunc        -- ^ The log function
    , appConnPool       :: !ConnectionPool -- ^ The connexion pool
    , appConfig         :: !Config         -- ^ The configuration file
    , appProcessContext :: !ProcessContext -- ^ Context to start processes
    }

-- | Constraint for functions doing sql requests
class HasConnPool env where
    connPoolL :: Lens' env ConnectionPool

instance HasConnPool App where
    connPoolL = lens appConnPool (\x y -> x { appConnPool = y })

instance HasLogFunc App where
    logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

-- | Constraint for functions needing access to the config
class HasConfig env where
    configL :: Lens' env Config

instance HasConfig App where
    configL = lens appConfig (\x y -> x { appConfig = y })

-- | Constraint for functions needing to start a process
instance HasProcessContext App where
    processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

-- | Print an exception
logException :: (MonadIO m, MonadReader env m, HasLogFunc env, Show a) => a -> m ()
logException = logError . displayShow

-- | Init the application and run the rio actions
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
                runRIO lf $ withPool $ \pool -> do
                    pc <- mkDefaultProcessContext
                    -- Initialize the application
                    let app = App { appLogFunc        = lf
                                  , appConnPool       = pool
                                  , appConfig         = config
                                  , appProcessContext = pc }

                    -- Run the app, handle exceptions
                    logDebug $ "Start app with config: " <> displayShow config
                    liftIO $ runRIO app $ catch actions (\e -> do
                        logError ("Error: " <> display (e :: SomeException))
                        liftIO exitFailure)
              where
                  withPool =
                      let dbConfig' = config ^. dbConfig
                          connectionStringTxt = dbConfig' ^. connectionString
                          connectionStringBS = encodeUtf8 connectionStringTxt
                          nbConnections' = dbConfig' ^. nbConnections
                       in case dbConfig' ^. backend of
                              Sqlite     -> withSqlitePool connectionStringTxt nbConnections'
                              Postgresql -> withPostgresqlPool connectionStringBS nbConnections'

-- | Run sql actions with the pool
runDB :: (HasConnPool env) => SqlPersistM a-> RIO env a
runDB actions = do
    pool <- view connPoolL
    liftIO $ runSqlPersistMPool actions pool

