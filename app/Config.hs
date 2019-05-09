{-# LANGUAGE TemplateHaskell #-}

module Config 
  ( Config(..), 
    getConfig
  )
where

import           RIO
import           Data.Yaml 
    ( FromJSON
    , ToJSON
    , encodeFile
    , decodeFileThrow
    )
import           Path 
    ( Abs
    , Dir
    , File
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


-- | Simple configuration stored in the config file
newtype Config = Config { db :: Path Abs File } 
    deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config

-- | Get a path from a file in the config directory
getFileInConfigDir :: MonadIO m => Path Rel t -> m (Path Abs t)
getFileInConfigDir file = flip (</>) file <$> getConfigDir

-- | Get the configuration directory
getConfigDir :: MonadIO m => m (Path Abs Dir)
getConfigDir = getXdgDir XdgConfig $ Just $(mkRelDir "hscalendar")

-- | Create a default configuration
defaultConfig :: MonadIO m => m Config
defaultConfig = do
    defaultDb <- getFileInConfigDir $(mkRelFile "database.db")
    return $ Config defaultDb 

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
    decodeFileThrow $ toFilePath fc

