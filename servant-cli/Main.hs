{-# OPTIONS_GHC -fno-warn-orphans #-}
import           RIO
import qualified RIO.Text                 as Text
import qualified RIO.Time                 as Time (toGregorian)

import           Control.Concurrent       (forkIO, killThread)
import           Control.Monad.Except     (ExceptT(..))
import           Data.Attoparsec.Text
    ( parseOnly
    , endOfInput
    )
import           Data.ByteString.Lazy.Char8 as DBLC (pack)
import           Data.Aeson (FromJSON, ToJSON)
import           Formatting (int, left, sformat, (%.))
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Network.Wai.Handler.Warp (run)
import           Options.Applicative (header, progDesc)
import           Servant.API              
    ( Get
    , Capture
    , Delete
    , JSON
    , Post
    , Put
    , ReqBody
    , Summary
    , FromHttpApiData(..)
    , ToHttpApiData(..)
    , (:>)
    , (:<|>)(..))
import           Servant.CLI              
    ( DocCapture(..)
    , ParseBody(..)
    , ToCapture(..)
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
import qualified App.CustomDay as CD (CustomDay(..), parser, toDay)
import           Db.Model 
    ( HdNotFound(..)
    , ProjExists(..)
    , ProjHasHd(..)
    , ProjNotFound(..)
    , hdGet
    , projAdd
    , projList
    , projRename
    , projRm
    )
import           Db.HalfDay (HalfDay(..))
import           Db.Project (Project, readProject, unProject)
import           Db.TimeInDay as TID (TimeInDay(..), parser)

instance ParseBody Project where
    parseBody = defaultParseBody "Project" readProject

data RenameArgs = MkRenameArgs { from :: Project, to :: Project }
    deriving (Eq, Generic, Show, Ord)

instance FromJSON RenameArgs
instance ToJSON RenameArgs

instance ParseBody RenameArgs where
    parseBody = MkRenameArgs <$> parseBody <*> parseBody

instance ToCapture (Capture "day" CD.CustomDay) where
    toCapture _ = DocCapture "day" "today|tomorrow|yesterday|22-03-1979|22-03|22"

instance FromHttpApiData CD.CustomDay where
    parseQueryParam text = case parseOnly (CD.parser <* endOfInput) text of
        Left err -> Left (Text.pack err)
        Right x  -> Right x

instance ToHttpApiData CD.CustomDay where
    toQueryParam (CD.MkDay day) = Text.intercalate "-" (fmap printNum [d, m, intY]) 
        where (y, m, d) = Time.toGregorian day
              intY = fromIntegral y
    toQueryParam (CD.MkDayNum d)        = printNum d        
    toQueryParam (CD.MkDayMonthNum d m) = Text.intercalate "-" (fmap printNum [d, m]) 
    toQueryParam CD.Today               = "today"   
    toQueryParam CD.Yesterday           = "yesterday"   
    toQueryParam CD.Tomorrow            = "tomorrow"

instance FromHttpApiData TimeInDay where
    parseQueryParam text = case parseOnly (TID.parser <* endOfInput) text of
        Left err -> Left (Text.pack err)
        Right x  -> Right x

instance ToHttpApiData TimeInDay where
    toQueryParam Morning   = "morning"
    toQueryParam Afternoon = "afternoon"

instance ToCapture (Capture "time in day" TimeInDay) where
    toCapture _ = DocCapture "time in day" "morning|afternoon"

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
   :<|> Summary "Rename a project"
           :> "project"
           :> "rename"
           :> ReqBody '[JSON] RenameArgs
           :> Put '[JSON] Project
   :<|> Summary "Display a half-day"
           :> Capture "day" CD.CustomDay
           :> Capture "time in day" TimeInDay
           :> Get '[JSON] HalfDay

printNum :: Int -> Text
printNum = sformat (left 2 '0' %. int) 

rioServer :: ServerT HSCalendarApi (RIO App)
rioServer =    allProjects 
          :<|> rmProject 
          :<|> addProject 
          :<|> renameProject
          :<|> displayHd

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

renameProject :: HasConnPool env => RenameArgs -> RIO env Project
renameProject (MkRenameArgs p1 p2) = catches (runDB $ projRename p1 p2 >> return p2)
    [ Handler (\e@(ProjExists _) -> throwM err409 { errBody = DBLC.pack $ show e } )
    , Handler (\(ProjNotFound _) -> throwM err404)
    ]

displayHd :: CD.CustomDay -> TimeInDay -> RIO App HalfDay
displayHd cd tid = do
    -- Get actual day
    day <- CD.toDay cd
    -- Get half-day
    try (runDB $ hdGet day tid) >>=
        \case
            Left (HdNotFound _ _) -> throwM err404
            Right hd -> return hd

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
               :<|> (\project -> "Project added: "   <> unProject project)
               :<|> (\project -> "Project renamed: " <> unProject project)
               :<|> Text.pack . show 
            
            res <- liftIO $ runClientM c (mkClientEnv manager (BaseUrl Http "localhost" 8081 ""))
            
            case res of
                Left e      -> throwIO e
                Right rtext -> logInfo $ display rtext
