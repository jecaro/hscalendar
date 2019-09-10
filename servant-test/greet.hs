{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

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
    , Handler
    , serve
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


hscalendarApi :: Proxy HSCalendarApi
hscalendarApi = Proxy

server :: Server.Application
server = Server.serve hscalendarApi $
        allProjects
   :<|> rmProject

allProjects :: Server.Handler Text
allProjects = return "list all the projects"

rmProject :: Server.Handler Text
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
