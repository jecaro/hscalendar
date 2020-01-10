import           RIO

import           Control.Monad.Except (ExceptT(..))
import           Data.ByteString.Lazy.Char8 as DBLC (pack)
import           Database.Persist.Sql (runMigration)
import           Network.Wai.Handler.Warp (run)
import           Servant.API.BasicAuth (BasicAuthData (BasicAuthData))
import           Servant.API (NoContent(..), (:<|>)(..))
import           Servant.Server
    ( Application
    , BasicAuthCheck (BasicAuthCheck)
    , BasicAuthResult(..)
    , Context ((:.), EmptyContext)
    , ServantErr(..)
    , Server
    , ServerT
    , err404
    , err409
    , err409
    , hoistServerWithContext
    , serveWithContext
    )
import qualified Servant.Server as Server (Handler(..))
import           System.Environment (lookupEnv)

import           App.App (App, HasConfig, HasConnPool, initAppAndRun, runDB)
import           App.API
    ( ProtectedHSCalendarApi
    , protectedHSCalendarApi
    , RenameArgs(..)
    )
import           App.CustomDay (CustomDay(..), toDay)
import           App.CustomWeek (CustomWeek(..), toWeek)
import           App.WorkOption
    ( ProjCmdIsMandatory(..)
    , runWorkOptions
    , WorkOption(..)
    )
import           Db.HalfDay (HalfDay(..))
import           Db.IdleDayType (IdleDayType(..))
import           Db.Login (Login, mkLogin)
import           Db.Model
    ( HdNotFound(..)
    , ProjExists(..)
    , ProjHasHd(..)
    , ProjNotFound(..)
    , TimesAreWrong(..)
    , UserNotFound(..)
    , hdGet
    , hdRm
    , hdSetHoliday
    , migrateAll
    , projAdd
    , projList
    , projRename
    , projRm
    , userCheck
    , weekGet
    )
import           Db.Password (mkPassword)
import           Db.Project (Project)
import           Db.TimeInDay (TimeInDay(..))

rioServer :: ServerT ProtectedHSCalendarApi (RIO App)
rioServer _ =  hMigrateAll
          :<|> hProjList
          :<|> hProjAdd
          :<|> hProjRm
          :<|> hProjRename
          :<|> hHdGet
          :<|> hWeekGet
          :<|> hHdSetIdleDay
          :<|> hHdSetWork
          :<|> hHdRm

authCheckInRIO :: HasConnPool env => BasicAuthData -> RIO env (BasicAuthResult Login)
authCheckInRIO (BasicAuthData authName authPass) = do
    let mbLogin = mkLogin $ decodeUtf8With lenientDecode authName
        mbPassword = mkPassword $ decodeUtf8With lenientDecode authPass
    case (mbLogin, mbPassword) of
        (Just login, Just password) -> do
            eiAuthenticated <- try $ runDB $ userCheck login password
            case eiAuthenticated of
                Left (UserNotFound _) -> return NoSuchUser
                Right authenticated -> if authenticated
                                           then return (Authorized login)
                                           else return BadPassword
        _ -> return Unauthorized

authCheck :: App -> BasicAuthCheck Login
authCheck app = BasicAuthCheck (runRIO app . authCheckInRIO)

mainServer :: App -> Server ProtectedHSCalendarApi
mainServer app = hoistServerWithContext protectedHSCalendarApi
    (Proxy :: Proxy '[BasicAuthCheck Login]) (nt app) rioServer

-- | https://www.parsonsmatt.org/2017/06/21/exceptional_servant_handling.html
nt :: App -> RIO App a -> Server.Handler a
nt app actions = Server.Handler . ExceptT . try $ runRIO app actions

server :: App -> Application
server app = serveWithContext protectedHSCalendarApi context (mainServer app)
    where context = authCheck app :. EmptyContext

hMigrateAll :: HasConnPool env => RIO env NoContent
hMigrateAll = runDB $ runMigration migrateAll >> return NoContent

hProjList :: HasConnPool env => RIO env [Project]
hProjList = runDB projList

hProjRm :: HasConnPool env => Project -> RIO env NoContent
hProjRm project = catches (runDB (projRm project) >> return NoContent)
        [ Handler (\e@(ProjHasHd _)  -> throwM err409 { errBody = DBLC.pack $ show e } )
        , Handler (\(ProjNotFound _) -> throwM err404)
        ]

hProjAdd :: HasConnPool env => Project -> RIO env NoContent
hProjAdd project = catch (runDB (projAdd project) >> return NoContent)
        (\e@(ProjExists _) -> throwM err409 { errBody = DBLC.pack $ show e } )

hProjRename :: HasConnPool env => RenameArgs -> RIO env NoContent
hProjRename (MkRenameArgs p1 p2) = catches (runDB $ projRename p1 p2 >> return NoContent)
    [ Handler (\e@(ProjExists _) -> throwM err409 { errBody = DBLC.pack $ show e } )
    , Handler (\(ProjNotFound _) -> throwM err404)
    ]

hHdGet :: HasConnPool env => CustomDay -> TimeInDay -> RIO env HalfDay
hHdGet cd tid = do
    -- Get actual day
    day <- toDay cd
    -- Get half-day
    try (runDB $ hdGet day tid) >>=
        \case
            Left (HdNotFound _ _) -> throwM err404
            Right hd -> return hd

hWeekGet :: HasConnPool env => CustomWeek -> RIO env [HalfDay]
hWeekGet cw = do
    -- Get actual week
    week <- toWeek cw
    -- Get the list of half-day
    runDB $ weekGet week

hHdSetIdleDay
    :: HasConnPool env
    => CustomDay -> TimeInDay -> IdleDayType -> RIO env NoContent
hHdSetIdleDay cd tid idt = do
    day <- toDay cd
    runDB $ hdSetHoliday day tid idt
    return NoContent

hHdRm :: HasConnPool env => CustomDay -> TimeInDay  -> RIO env NoContent
hHdRm cd tid = do
    day <- toDay cd
    catch (runDB (hdRm day tid) >> return NoContent)
        (\(HdNotFound _ _) -> throwM err404)

hHdSetWork
    :: (HasConnPool env, HasConfig env)
    => CustomDay -> TimeInDay -> [WorkOption] -> RIO env NoContent
hHdSetWork cd tid wopts = do
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
