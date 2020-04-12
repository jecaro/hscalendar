import           RIO

import           Control.Monad.Except (ExceptT(..))
import           Data.ByteString.Lazy.Char8 as Char8 (pack)
import           Data.ByteString.Lazy as Lazy
import           Data.Default (def)
import           Database.Persist.Sql (runMigration)
import           Network.HTTP.Media ((//), (/:))
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.RequestLogger
    ( IPAddrSource (..)
    , OutputFormat (..)
    , logStdoutDev
    , mkRequestLogger
    , outputFormat
    )
import           Options.Applicative
    ( Parser
    , ParserInfo
    , auto
    , execParser
    , help
    , helper
    , idm
    , info
    , long
    , metavar
    , option
    , short
    , switch
    , value
    , (<**>)
    )
import           Servant.API.BasicAuth (BasicAuthData (BasicAuthData))
import           Servant.API 
    ( Accept(..)
    , Get
    , MimeRender(..)
    , NoContent(..)
    , Raw
    , Summary
    , (:<|>)(..)
    , (:>)
    )
import           Servant.Server
    ( Application
    , BasicAuthCheck (BasicAuthCheck)
    , BasicAuthResult(..)
    , Context ((:.), EmptyContext)
    , Server
    , ServerError(..)
    , ServerT
    , err404
    , err409
    , err409
    , hoistServerWithContext
    , serveWithContext
    )
import qualified Servant.Server as Server (Handler(..))
import           Servant.Server.StaticFiles (serveDirectoryWebApp)

import           App.App (App, HasConfig, HasConnPool, initAppAndRun, runDB)
import           App.Api (HSBasicAuth, HSCalendarApi, RenameArgs(..))
import           App.DayDesc (DayDesc(..), toDay)
import           App.MonthDesc (MonthDesc(..), toMonth)
import           App.WeekDesc (WeekDesc(..), toWeek)
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
    , monthGet
    , projAdd
    , projList
    , projRename
    , projRm
    , userCheck
    , weekGet
    )
import           Db.MonthF (MonthWithDays)
import           Db.Password (mkPassword)
import           Db.Project (Project)
import           Db.TimeInDay (TimeInDay(..))
import           Db.WeekF (WeekWithDays)

data HTML

newtype RawHtml = RawHtml { unRaw :: Lazy.ByteString }

instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
    mimeRender _ = unRaw

type ProtectedApi 
    = HSBasicAuth 
    :> ( HSCalendarApi 
        :<|> Summary "Root" :> Get '[HTML] RawHtml 
        :<|> Summary "Directory" :> Raw
       )

protectedApi :: Proxy ProtectedApi
protectedApi = Proxy

protectedRioServer :: ServerT ProtectedApi (RIO App)
protectedRioServer _
    = rioServer 
    :<|> serveRoot
    :<|> serveDirectoryWebApp "frontend"

serveRoot :: RIO App RawHtml
serveRoot = fmap RawHtml (liftIO $ Lazy.readFile "frontend/index.html")

rioServer :: ServerT HSCalendarApi (RIO App)
rioServer =  hMigrateAll
                   :<|> hProjList
                   :<|> hProjAdd
                   :<|> hProjRm
                   :<|> hProjRename
                   :<|> hHdGet
                   :<|> hWeekGet
                   :<|> hMonthGet
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
                Left (UserNotFound _) -> pure NoSuchUser
                Right authenticated -> if authenticated
                                           then pure (Authorized login)
                                           else pure BadPassword
        _ -> pure Unauthorized

authCheck :: App -> BasicAuthCheck Login
authCheck app = BasicAuthCheck (runRIO app . authCheckInRIO)

mainServer :: App -> Server ProtectedApi
mainServer app = hoistServerWithContext protectedApi
    (Proxy :: Proxy '[BasicAuthCheck Login]) (nt app) protectedRioServer

-- | https://www.parsonsmatt.org/2017/06/21/exceptional_servant_handling.html
nt :: App -> RIO App a -> Server.Handler a
nt app actions = Server.Handler . ExceptT . try $ runRIO app actions

server :: App -> Application
server app = serveWithContext protectedApi context (mainServer app)
    where context = authCheck app :. EmptyContext

hMigrateAll :: HasConnPool env => RIO env NoContent
hMigrateAll = runDB $ runMigration migrateAll >> pure NoContent

hProjList :: HasConnPool env => RIO env [Project]
hProjList = runDB projList

hProjRm :: HasConnPool env => Project -> RIO env NoContent
hProjRm project = catches (runDB (projRm project) >> pure NoContent)
        [ Handler (\e@(ProjHasHd _)  -> throwM err409 { errBody = Char8.pack $ show e } )
        , Handler (\(ProjNotFound _) -> throwM err404)
        ]

hProjAdd :: HasConnPool env => Project -> RIO env NoContent
hProjAdd project = catch (runDB (projAdd project) >> pure NoContent)
        (\e@(ProjExists _) -> throwM err409 { errBody = Char8.pack $ show e } )

hProjRename :: HasConnPool env => RenameArgs -> RIO env NoContent
hProjRename (MkRenameArgs p1 p2) = catches (runDB $ projRename p1 p2 >> pure NoContent)
    [ Handler (\e@(ProjExists _) -> throwM err409 { errBody = Char8.pack $ show e } )
    , Handler (\(ProjNotFound _) -> throwM err404)
    ]

hHdGet :: HasConnPool env => DayDesc -> TimeInDay -> RIO env HalfDay
hHdGet cd tid = do
    -- Get actual day
    day <- toDay cd
    -- Get half-day
    try (runDB $ hdGet day tid) >>=
        \case
            Left (HdNotFound _ _) -> throwM err404
            Right hd -> pure hd

hWeekGet :: HasConnPool env => WeekDesc -> RIO env WeekWithDays
hWeekGet cw = do
    -- Get actual week
    week <- toWeek cw
    -- Get the list of half-day
    runDB $ weekGet week

hMonthGet :: HasConnPool env => MonthDesc -> RIO env MonthWithDays
hMonthGet cm = do
    -- Get actual month
    month <- toMonth cm
    -- Get the list of half-day
    runDB $ monthGet month

hHdSetIdleDay
    :: HasConnPool env
    => DayDesc -> TimeInDay -> IdleDayType -> RIO env NoContent
hHdSetIdleDay cd tid idt = do
    day <- toDay cd
    runDB $ hdSetHoliday day tid idt
    pure NoContent

hHdRm :: HasConnPool env => DayDesc -> TimeInDay  -> RIO env NoContent
hHdRm cd tid = do
    day <- toDay cd
    catch (runDB (hdRm day tid) >> pure NoContent)
        (\(HdNotFound _ _) -> throwM err404)

hHdSetWork
    :: (HasConnPool env, HasConfig env)
    => DayDesc -> TimeInDay -> [WorkOption] -> RIO env NoContent
hHdSetWork cd tid wopts = do
    day <- toDay cd
    -- Create the record in DB
    catches (runWorkOptions day tid wopts >> pure NoContent)
        [ Handler (\e@TimesAreWrong      -> throwM err409 { errBody = Char8.pack $ show e } )
        , Handler (\e@ProjCmdIsMandatory -> throwM err409 { errBody = Char8.pack $ show e } )
        , Handler (\(ProjNotFound _)     -> throwM err404)
        ]

-- | optparse options
data Options = Options
    Bool -- ^ Verbose mode for wai logging
    Int  -- ^ Server port

options :: Parser Options
options = Options
    <$> switch (long "verbose" <> short 'v' <> help "Verbose output")
    <*> option auto
        (  long "port"
        <> short 'p'
        <> metavar "PORT"
        <> help "server port"
        <> value 8081
        )

optionsInfo :: ParserInfo Options
optionsInfo = info (options <**> helper) idm

main :: IO ()
main = do
    (Options verbose port) <- execParser optionsInfo
    initAppAndRun False LevelInfo $ do
        app <- ask
        logger <-
            if verbose
                then pure logStdoutDev
                else liftIO $ mkRequestLogger def { outputFormat = Apache FromHeader }
        logInfo $ "Start server on port: " <> display port
        liftIO . run port $ logger (server app)
