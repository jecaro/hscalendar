{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE FlexibleContexts #-}

import           RIO
import qualified RIO.Text                 as Text

import           Control.Concurrent       (forkIO, killThread)
import           Data.Text                (Text)
import           Network.HTTP.Client      (newManager, defaultManagerSettings)
import           Network.Wai.Handler.Warp (run)
import           Options.Applicative      (header, progDesc)
import           Servant.API              (Get, JSON, Summary, (:>), (:<|>)(..))
import           Servant.CLI              (parseHandleClient)
import           Servant.Client           
    ( BaseUrl(..)
    , ClientM
    , Scheme(Http)
    , mkClientEnv
    , runClientM
    )
import qualified Servant.Server           as Server 
    ( Application
    , serve
    , ServerT
    , Server
    , hoistServer
    )


type HSCalendarApi =
        Summary "List all projects"
           :> "project"
           :> "all"
           :> Get '[JSON] Text
   :<|> Summary "Rm project"
           :> "project"
           :> "rm"
           :> Get '[JSON] Text


rioServer :: Server.ServerT HSCalendarApi (RIO Text)
rioServer = allProjects :<|> rmProject 

hscalendarApi :: Proxy HSCalendarApi
hscalendarApi = Proxy

mainServer :: Server.Server HSCalendarApi
mainServer = Server.hoistServer hscalendarApi nt rioServer

nt :: (MonadIO m) => RIO Text a -> m a
nt = runRIO "hi" 

server :: Server.Application
server = Server.serve hscalendarApi mainServer

allProjects :: RIO Text Text
allProjects = do
    env <- ask
    return $ "list all the projects with env " <> env

rmProject :: RIO Text Text
rmProject = return "rm a project"

withServer :: IO () -> IO ()
withServer actions = bracket (forkIO $ run 8081 server) killThread (const actions)

main :: IO ()
main = withServer $ do

    manager <- newManager defaultManagerSettings
    
    runSimpleApp $ do
        c <- liftIO $ parseHandleClient
                        hscalendarApi
                        (Proxy :: Proxy ClientM)
                        (header "hscalendar" <> progDesc "hscalendar API") $
                Text.unpack 
           :<|> Text.unpack 
    
        res <- liftIO $ runClientM c (mkClientEnv manager (BaseUrl Http "localhost" 8081 ""))
    
        case res of
          Left e        -> throwIO e
          Right rstring -> logInfo $ displayShow rstring
