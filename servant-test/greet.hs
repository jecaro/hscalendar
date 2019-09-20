{-# OPTIONS_GHC -fno-warn-orphans #-}
import           RIO
import qualified RIO.Text                 as Text

import           Control.Concurrent       (forkIO, killThread)
import           Control.Monad.Except     (ExceptT(..))
import           Data.ByteString.Lazy.Char8 as DBLC (pack)
import           Network.HTTP.Client      (newManager, defaultManagerSettings)
import           Network.Wai.Handler.Warp (run)
import           Options.Applicative      (header, ReadM, maybeReader, progDesc)
import           Servant.API              
    ( Get
    , Delete
    , JSON
    , Post
    , ReqBody
    , Summary
    , (:>)
    , (:<|>)(..))
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
import           Servant.Server          
    ( Application
    , ServantErr(..)
    , Server
    , ServerT
    , err404
    , err409
    , err409
    , hoistServer
    , serve
    )
import qualified Servant.Server as Server (Handler(..))         

import           App.App (App, HasConnPool, initAppAndRun, runDB)
import           Db.Model 
    ( ProjExists(..)
    , ProjHasHd(..)
    , ProjNotFound(..)
    , projAdd
    , projList
    , projRm
    )
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
           :> Delete '[JSON] ()
   :<|> Summary "Add a project"
           :> "project"
           :> "add"
           :> ReqBody '[JSON] Project
           :> Post '[JSON] Project


rioServer :: ServerT HSCalendarApi (RIO App)
rioServer = allProjects :<|> rmProject :<|> addProject

hscalendarApi :: Proxy HSCalendarApi
hscalendarApi = Proxy

mainServer :: App -> Server HSCalendarApi
mainServer app = hoistServer hscalendarApi (nt app) rioServer

-- https://www.parsonsmatt.org/2017/06/21/exceptional_servant_handling.html
nt :: App -> RIO App a -> Server.Handler a
nt app actions = Server.Handler . ExceptT . try $ runRIO app actions

server :: App -> Application
server app = serve hscalendarApi (mainServer app)

allProjects :: HasConnPool env => RIO env [Project]
allProjects = runDB projList 

rmProject :: HasConnPool env => Project -> RIO env ()
rmProject project = catches (runDB (projRm project)) 
        [ Handler (\e@(ProjHasHd _)  -> throwM err409 { errBody = DBLC.pack $ show e } )
        , Handler (\(ProjNotFound _) -> throwM err404)
        ]

addProject :: HasConnPool env => Project -> RIO env Project
addProject project = catch (runDB (projAdd project) >> return project) 
        (\e@(ProjExists _) -> throwM err409 { errBody = DBLC.pack $ show e } )

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
               :<|> const "Project deleted"
               :<|> \project -> "Project added: " <> unProject project 
            
            res <- liftIO $ runClientM c (mkClientEnv manager (BaseUrl Http "localhost" 8081 ""))
            
            case res of
                Left e      -> throwIO e
                Right rtext -> logInfo $ display rtext
