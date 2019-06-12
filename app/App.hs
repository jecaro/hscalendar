module App
    ( App(..)
    , HasConnPool(..)
    , HasConfig(..)
    )
where

import           RIO

import           Database.Persist.Sql (ConnectionPool)

import           Config(Config(..))

-- | The app data
data App = App
    { appLogFunc  :: !LogFunc        -- ^ The log function
    , appConnPool :: !ConnectionPool -- ^ The connexion pool
    , appConfig   :: !Config         -- ^ The configuration file
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