import           RIO

import           Control.Monad.Except (ExceptT(..))
import           Data.Aeson (FromJSON, ToJSON)
import           Data.ByteString.Lazy.Char8 as DBLC (pack)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
    ( Get
    , Capture
    , DeleteNoContent
    , JSON
    , NoContent(..)
    , PostNoContent
    , PutNoContent
    , ReqBody
    , Summary
    , (:>)
    , (:<|>)(..))
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
import           System.Environment (lookupEnv)

import           App.App (App, HasConfig, HasConnPool, initAppAndRun, runDB)
import           App.CustomDay (CustomDay(..), toDay)
import           App.WorkOption
    ( ProjCmdIsMandatory(..)
    , runWorkOptions
    , WorkOption(..)
    )
import           Db.HalfDay (HalfDay(..))
import           Db.IdleDayType (IdleDayType(..))
import           Db.Model
    ( HdNotFound(..)
    , ProjExists(..)
    , ProjHasHd(..)
    , ProjNotFound(..)
    , hdGet
    , hdRm
    , hdSetHoliday
    , projAdd
    , projList
    , projRename
    , projRm
    , TimesAreWrong(..)
    )
import           Db.Project (Project)
import           Db.TimeInDay (TimeInDay(..))


data RenameArgs = MkRenameArgs { from :: Project, to :: Project }
    deriving (Eq, Generic, Show, Ord)

instance FromJSON RenameArgs
instance ToJSON RenameArgs

type HSCalendarApi =
        Summary "List all projects"
           :> "project"
           :> Get '[JSON] [Project]
   :<|> Summary "Add a project"
           :> "project"
           :> ReqBody '[JSON] Project
           :> PostNoContent '[JSON] NoContent
   :<|> Summary "Delete a project"
           :> "project"
           :> ReqBody '[JSON] Project
           :> DeleteNoContent '[JSON] NoContent
   :<|> Summary "Rename a project"
           :> "project"
           :> ReqBody '[JSON] RenameArgs
           :> PutNoContent '[JSON] NoContent
   :<|> Summary "Display a half-day"
           :> "diary"
           :> Capture "day" CustomDay
           :> Capture "time in day" TimeInDay
           :> Get '[JSON] HalfDay
   :<|> Summary "Set a non-working half-day"
           :> "diary"
           :> "idle"
           :> Capture "day" CustomDay
           :> Capture "time in day" TimeInDay
           :> ReqBody '[JSON] IdleDayType
           :> PutNoContent '[JSON] NoContent
   :<|> Summary "Set a working half-day"
           :> "diary"
           :> Capture "day" CustomDay
           :> Capture "time in day" TimeInDay
           :> ReqBody '[JSON] [WorkOption]
           :> PutNoContent '[JSON] NoContent
   :<|> Summary "Delete a half-day"
           :> "diary"
           :> Capture "day" CustomDay
           :> Capture "time in day" TimeInDay
           :> DeleteNoContent '[JSON] NoContent

rioServer :: ServerT HSCalendarApi (RIO App)
rioServer =    projectAll
          :<|> projectAdd
          :<|> projectRm
          :<|> projectRename
          :<|> diaryDisplay
          :<|> diarySetIdleDay
          :<|> diarySetWork
          :<|> diaryRm

hscalendarApi :: Proxy HSCalendarApi
hscalendarApi = Proxy

mainServer :: App -> Server HSCalendarApi
mainServer app = hoistServer hscalendarApi (nt app) rioServer

-- | https://www.parsonsmatt.org/2017/06/21/exceptional_servant_handling.html
nt :: App -> RIO App a -> Server.Handler a
nt app actions = Server.Handler . ExceptT . try $ runRIO app actions

server :: App -> Application
server app = serve hscalendarApi (mainServer app)

projectAll :: HasConnPool env => RIO env [Project]
projectAll = runDB projList

projectRm :: HasConnPool env => Project -> RIO env NoContent
projectRm project = catches (runDB (projRm project) >> return NoContent)
        [ Handler (\e@(ProjHasHd _)  -> throwM err409 { errBody = DBLC.pack $ show e } )
        , Handler (\(ProjNotFound _) -> throwM err404)
        ]

projectAdd :: HasConnPool env => Project -> RIO env NoContent
projectAdd project = catch (runDB (projAdd project) >> return NoContent)
        (\e@(ProjExists _) -> throwM err409 { errBody = DBLC.pack $ show e } )

projectRename :: HasConnPool env => RenameArgs -> RIO env NoContent
projectRename (MkRenameArgs p1 p2) = catches (runDB $ projRename p1 p2 >> return NoContent)
    [ Handler (\e@(ProjExists _) -> throwM err409 { errBody = DBLC.pack $ show e } )
    , Handler (\(ProjNotFound _) -> throwM err404)
    ]

diaryDisplay :: HasConnPool env => CustomDay -> TimeInDay -> RIO env HalfDay
diaryDisplay cd tid = do
    -- Get actual day
    day <- toDay cd
    -- Get half-day
    try (runDB $ hdGet day tid) >>=
        \case
            Left (HdNotFound _ _) -> throwM err404
            Right hd -> return hd

diarySetIdleDay
    :: HasConnPool env
    => CustomDay -> TimeInDay -> IdleDayType -> RIO env NoContent
diarySetIdleDay cd tid idt = do
    day <- toDay cd
    runDB $ hdSetHoliday day tid idt
    return NoContent

diaryRm :: HasConnPool env => CustomDay -> TimeInDay  -> RIO env NoContent
diaryRm cd tid = do
    day <- toDay cd
    catch (runDB (hdRm day tid) >> return NoContent)
        (\(HdNotFound _ _) -> throwM err404)

diarySetWork
    :: (HasConnPool env, HasConfig env)
    => CustomDay -> TimeInDay -> [WorkOption] -> RIO env NoContent
diarySetWork cd tid wopts = do
    day <- toDay cd
    -- Create the record in DB
    catches (runWorkOptions day tid wopts >> return NoContent)
        [ Handler (\e@TimesAreWrong      -> throwM err409 { errBody = DBLC.pack $ show e } )
        , Handler (\e@ProjCmdIsMandatory -> throwM err409 { errBody = DBLC.pack $ show e } )
        , Handler (\(ProjNotFound _)     -> throwM err404)
        ]

getPortFromEnv :: RIO env Int
getPortFromEnv = do
    maybePortStr <- liftIO $ lookupEnv "PORT"
    let maybePortInt = readMaybe =<< maybePortStr
    return $ fromMaybe 8081 maybePortInt

main :: IO ()
main = initAppAndRun False LevelInfo $ do
    port <- getPortFromEnv
    app <- ask
    liftIO . run port $ server app
