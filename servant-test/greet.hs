{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

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

import           App.App (App, HasConnPool, initAppAndRun, runDB)
import           Db.Model (projList)
import           Db.Project (Project, unProject)

type HSCalendarApi =
        Summary "List all projects"
           :> "project"
           :> "all"
           :> Get '[JSON] [Project]
   :<|> Summary "Rm project"
           :> "project"
           :> "rm"
           :> Get '[JSON] Text


rioServer :: Server.ServerT HSCalendarApi (RIO App)
rioServer = allProjects :<|> rmProject 

hscalendarApi :: Proxy HSCalendarApi
hscalendarApi = Proxy

mainServer :: App -> Server.Server HSCalendarApi
mainServer app = Server.hoistServer hscalendarApi (runRIO app) rioServer

server :: App -> Server.Application
server app = Server.serve hscalendarApi (mainServer app)

allProjects :: HasConnPool env => RIO env [Project]
allProjects = runDB projList 

rmProject :: RIO App Text
rmProject = return "rm a project"

withServer :: MonadUnliftIO m => App -> m c -> m c
withServer app actions = bracket (liftIO $ forkIO $ run 8081 $ server app) 
    (liftIO . killThread) (const actions)

main :: IO ()
main = do

    manager <- newManager defaultManagerSettings

    initAppAndRun False LevelInfo $ do
    
        app <- ask
        withServer app $ do

            c <- liftIO $ parseHandleClient
                            hscalendarApi
                            (Proxy :: Proxy ClientM)
                            (header "hscalendar" <> progDesc "hscalendar API") $
                    Text.unlines . map unProject 
               :<|> id 
            
            res <- liftIO $ runClientM c (mkClientEnv manager (BaseUrl Http "localhost" 8081 ""))
            
            case res of
                Left e      -> throwIO e
                Right rtext -> logInfo $ display rtext
