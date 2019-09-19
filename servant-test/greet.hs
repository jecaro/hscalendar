import           RIO
import qualified RIO.Text                 as Text

import           Control.Concurrent       (forkIO, killThread)
import           Control.Monad.Except     (ExceptT(..))
import           Data.ByteString.Lazy.Char8 as DBLC (pack)
import           Network.HTTP.Client      (newManager, defaultManagerSettings)
import           Network.Wai.Handler.Warp (run)
import           Options.Applicative      (header, ReadM, maybeReader, progDesc)
import           Servant.API              (Get, JSON, ReqBody, Summary, (:>), (:<|>)(..))
import           Servant.CLI              
    ( ParseBody(..)
    , defaultParseBody
    , parseBody
    , parseHandleClient
    )
import           Servant.Client           
    ( BaseUrl(..)
    , ClientM
    , Scheme(Http)
    , mkClientEnv
    , runClientM
    )
import qualified Servant.Server           as Server 
    ( Application
    , Handler(..)
    , ServantErr(..)
    , Server
    , ServerT
    , err404
    , err409
    , err409
    , hoistServer
    , serve
    )

import           App.App (App, HasConnPool, initAppAndRun, runDB)
import           Db.Model (ProjNotFound(..), ProjHasHd(..), projList, projRm)
import           Db.Project (Project, mkProject, unProject)

instance ParseBody Project where
    parseBody = defaultParseBody "Project" readProject

readProject :: ReadM Project
readProject = maybeReader $ mkProject . Text.pack

type HSCalendarApi =
        Summary "List all projects"
           :> "project"
           :> "all"
           :> Get '[JSON] [Project]
   :<|> Summary "Rm project"
           :> "project"
           :> "rm"
           :> ReqBody '[JSON] Project
           :> Get '[JSON] Text


rioServer :: Server.ServerT HSCalendarApi (RIO App)
rioServer = allProjects :<|> rmProject 

hscalendarApi :: Proxy HSCalendarApi
hscalendarApi = Proxy

mainServer :: App -> Server.Server HSCalendarApi
mainServer app = Server.hoistServer hscalendarApi (nt app) rioServer

-- https://www.parsonsmatt.org/2017/06/21/exceptional_servant_handling.html
nt :: App -> RIO App a -> Server.Handler a
nt app actions = Server.Handler . ExceptT . try $ runRIO app actions

server :: App -> Server.Application
server app = Server.serve hscalendarApi (mainServer app)

allProjects :: HasConnPool env => RIO env [Project]
allProjects = runDB projList 

rmProject :: HasConnPool env => Project -> RIO env Text
rmProject project = do
    res <- catches (runDB (projRm project) >> return "Project deleted") 
        [ Handler (\e@(ProjHasHd _)  -> throwM Server.err409 { Server.errBody = DBLC.pack $ show e } )
        , Handler (\(ProjNotFound _) -> throwM Server.err404)
        ]
    return $ Text.pack res

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
