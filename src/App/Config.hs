-- | The configuration file in yaml format
{-# LANGUAGE TemplateHaskell #-}

module App.Config
    ( afternoon
    , arrived
    , backend
    , Config(..)
    , connectionString
    , DbBackend(..)
    , dbConfig
    , DbConfig(..)
    , defaultHours
    , DefaultHours(..)
    , DefaultHoursForDay(..)
    , defaultOffice
    , getConfig
    , left
    , morning
    , nbConnections
    )
where

import           RIO
import qualified RIO.Text as Text
import qualified RIO.Time as Time (TimeOfDay(..))

import           Data.Attoparsec.Text
    ( asciiCI
    , endOfInput
    , parseOnly
    , Parser
    )
import           Data.Either.Combinators (rightToMaybe)
import           Data.Yaml
    ( FromJSON
    , ToJSON
    , encodeFile
    , decodeFileThrow
    )
import           Lens.Micro.Platform (makeFields, (%~))
import           Path
    ( Abs
    , Dir
    , Path
    , Rel
    , mkRelDir
    , mkRelFile
    , toFilePath
    , (</>)
    )
import           Path.IO
    ( XdgDirectory(XdgConfig)
    , doesFileExist
    , ensureDir
    , getXdgDir
    )
import           System.Environment (lookupEnv)
import           Text.Read (readMaybe)

import           Db.Office(Office(..))

-- | Two default hours for a half-day worked
data DefaultHours = DefaultHours { _defaultHoursArrived :: !Time.TimeOfDay
                                 , _defaultHoursLeft    :: !Time.TimeOfDay }
    deriving (Show, Generic)
makeFields ''DefaultHours

instance FromJSON DefaultHours
instance ToJSON DefaultHours

-- | The default hours for a full day worked
data DefaultHoursForDay = DefaultHoursForDay
    { _defaultHoursForDayMorning   :: !DefaultHours
    , _defaultHoursForDayAfternoon :: !DefaultHours }
    deriving (Show, Generic)
makeFields ''DefaultHoursForDay

instance FromJSON DefaultHoursForDay
instance ToJSON DefaultHoursForDay

-- | Database backends supported
data DbBackend = Sqlite | Postgresql
    deriving (Show, Generic)

instance FromJSON DbBackend
instance ToJSON DbBackend

-- | Settings for database access
data DbConfig = DBConfig { _dbConfigBackend          :: !DbBackend
                         , _dbConfigConnectionString :: !Text
                         , _dbConfigNbConnections    :: !Int}
    deriving (Show, Generic)
makeFields ''DbConfig

instance FromJSON DbConfig
instance ToJSON DbConfig

-- | Simple configuration stored in the config file
data Config = Config { _configDbConfig      :: !DbConfig
                     , _configDefaultHours  :: !DefaultHoursForDay
                     , _configDefaultOffice :: !Office
                     }
    deriving (Show, Generic)
makeFields ''Config

instance FromJSON Config
instance ToJSON Config

-- | Parser for a  backend
parser :: Parser DbBackend
parser =   asciiCI "sqlite"     $> Sqlite
       <|> asciiCI "postgresql" $> Postgresql

-- | Get a path from a file in the config directory
getFileInConfigDir :: MonadIO m => Path Rel t -> m (Path Abs t)
getFileInConfigDir file = flip (</>) file <$> getConfigDir

-- | Get the configuration directory
getConfigDir :: MonadIO m => m (Path Abs Dir)
getConfigDir = getXdgDir XdgConfig $ Just $(mkRelDir "hscalendar")

-- | Create a default configuration
defaultConfig :: MonadIO m => m Config
defaultConfig = do
    defaultSqliteFile <- getFileInConfigDir $(mkRelFile "database.db")
    let morning'   = DefaultHours (Time.TimeOfDay 8 20 0)  (Time.TimeOfDay 12 0 0)
        afternoon' = DefaultHours (Time.TimeOfDay 13 30 0) (Time.TimeOfDay 17 0 0)
    return $ Config { _configDbConfig =
                          DBConfig { _dbConfigBackend          = Sqlite
                                   , _dbConfigConnectionString = Text.pack $ toFilePath defaultSqliteFile
                                   , _dbConfigNbConnections    = 1
                                   }
                    , _configDefaultHours = DefaultHoursForDay morning' afternoon'
                    , _configDefaultOffice = Rennes
                    }

-- | Read the configuration from the config file, create it if it doesn't exist
getConfig :: HasLogFunc m => RIO m Config
getConfig = do
    -- Create config file directory
    getConfigDir >>= ensureDir
    -- Config file
    fc <- getFileInConfigDir $(mkRelFile "config.yml")
    -- Create it if it doesn't exist
    exists <- doesFileExist fc
    unless exists (do
        logDebug "The config file doesn't exist, create it."
        liftIO $ defaultConfig >>= encodeFile (toFilePath fc))
    -- Read config from the file
    config <- decodeFileThrow $ toFilePath fc
    -- Override with environment variables - the connection string
    mbDatabaseUrlStr <- liftIO $ lookupEnv "DATABASE_URL"
    let mbDatabaseUrl = Text.pack <$> mbDatabaseUrlStr
    -- The backend
    mbBackendStr <- liftIO $ lookupEnv "BACKEND"
    let mbBackendTxt = Text.pack <$> mbBackendStr
        mbBackend = (rightToMaybe . parseOnly (parser <* endOfInput)) =<< mbBackendTxt
    -- And the number of connections
    mbNbConnectionsStr <- liftIO $ lookupEnv "NB_CONNECTIONS"
    let mbNbConnections = readMaybe =<< mbNbConnectionsStr
    -- Update the config with that
    return $ config & dbConfig %~ (connectionString %~ (`fromMaybe` mbDatabaseUrl))
                                . (backend          %~ (`fromMaybe` mbBackend))
                                . (nbConnections    %~ (`fromMaybe` mbNbConnections))

