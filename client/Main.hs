{-# LANGUAGE RecordWildCards #-}

import           RIO
import           RIO.Process (HasProcessContext(..), ProcessContext, mkDefaultProcessContext)

import           Control.Applicative (many)
import           Data.Attoparsec.Text as Att
    ( Parser
    , decimal
    , letter
    , digit
    , char
    )
import           Options.Applicative as Opt
    ( Parser
    , ParserInfo
    , ReadM
    , argument
    , execParser
    , helper
    , idm
    , info
    , metavar
    , (<**>)
    , (<|>)
    )
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.Status.Extended ()
import           Network.HTTP.Types.Header.Extended ()
import           Servant.API (NoContent, (:<|>)(..), (:>))
import           Servant.API.BasicAuth (BasicAuthData(..))
import qualified Servant.API.BasicAuth.Extended as BAE (parse)
import           Servant.Client
    ( BaseUrl(..)
    , ClientEnv
    , ClientError(..)
    , ClientM
    , Scheme(..)
    , client
    , hoistClient
    , mkClientEnv
    , runClientM
    )
import qualified Servant.Client.Extended as CE (parse)

import           App.Api (HSBasicAuth, HSCalendarApi, RenameArgs(..))
import           App.CustomDay (CustomDay)
import           App.CustomWeek (CustomWeek)
import           App.CommandLine
    ( Cmd(..)
    , Options(..)
    , attoReadM
    , cmd
    , options
    )
import           App.Editor (editorToOptions)
import           App.WorkOption (WorkOption)
import           Db.FullDay (FullDay)
import           Db.FullWeek (FullWeek)
import           Db.HalfDay (HalfDay, displayHdWithDate)
import           Db.IdleDayType (IdleDayType)
import           Db.Project (Project)
import           Db.TimeInDay (TimeInDay)

type ProtectedHSCalendarApi = HSBasicAuth :> HSCalendarApi

protectedHSCalendarApi :: Proxy ProtectedHSCalendarApi
protectedHSCalendarApi = Proxy

data App = App
    { appLogFunc        :: !LogFunc        -- ^ The log function
    , appProcessContext :: !ProcessContext -- ^ Context to start processes
    }

instance HasLogFunc App where
    logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

-- | Constraint for functions needing to start a process
instance HasProcessContext App where
    processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })


-- | The main API, available once authentication is passed
data ProtectedClient env = ProtectedClient
    { migrateAll :: RIO env NoContent
    , projList :: RIO env [Project]
    , projAdd :: Project -> RIO env NoContent
    , projRm :: Project -> RIO env NoContent
    , projRename :: RenameArgs -> RIO env NoContent
    , hdGet :: CustomDay -> TimeInDay -> RIO env HalfDay
    , weekGet :: CustomWeek -> RIO env (FullWeek (FullDay (Maybe HalfDay)))
    , hdSetIdleDay :: CustomDay -> TimeInDay -> IdleDayType -> RIO env NoContent
    , hdSetWork :: CustomDay -> TimeInDay -> [WorkOption] -> RIO env NoContent
    , hdRm :: CustomDay -> TimeInDay -> RIO env NoContent
    }

-- | Init the API with the client env and auth data
mkProtectedApi :: ClientEnv -> BasicAuthData -> ProtectedClient a
mkProtectedApi env ad =
  let migrateAll
        :<|> projList
        :<|> projAdd
        :<|> projRm
        :<|> projRename
        :<|> hdGet
        :<|> weekGet
        :<|> hdSetIdleDay
        :<|> hdSetWork
        :<|> hdRm
        = hoistClient protectedHSCalendarApi (nt env) (client protectedHSCalendarApi) ad
  in
    ProtectedClient{..}

-- | Natural transformation between ClientM and RIO
nt :: ClientEnv -> ClientM a -> RIO env a
nt clientEnv actions = liftIO (runClientM actions clientEnv) >>=
    \case
        Left err -> throwIO err
        Right res -> return res

baseUrlAndBasicAuthData :: Opt.Parser (BaseUrl, BasicAuthData)
baseUrlAndBasicAuthData = argument readUrlAndAuthData (metavar "URL...")

parseBaseUrlAndAuthData :: Att.Parser (BaseUrl, BasicAuthData)
parseBaseUrlAndAuthData = do
    scheme <- CE.parse
    authData <- BAE.parse
    _ <- char '@'
    host <- many (letter <|> digit <|> char '.' <|> char '-')
    _ <- char ':'
    port <- decimal
    let baseUrl = BaseUrl scheme host port ""
    return (baseUrl, authData)

readUrlAndAuthData :: ReadM (BaseUrl, BasicAuthData)
readUrlAndAuthData = attoReadM parseBaseUrlAndAuthData

optionsAndURI :: Opt.Parser (Options, BaseUrl, BasicAuthData, Cmd)
optionsAndURI = rewrap <$> options <*> baseUrlAndBasicAuthData <*> cmd
    where rewrap x (y, y') z = (x, y, y', z)

optionsInfo :: ParserInfo (Options, BaseUrl, BasicAuthData, Cmd)
optionsInfo = info (optionsAndURI <**> helper) idm

handleError :: (HasLogFunc env) => ClientError -> RIO env ()
handleError (FailureResponse _ response) = logError $
    "The server returns an error\n" <> display response
handleError (ConnectionError text) = logError $
    "Unable to connect to the server\n" <> display text
handleError e = logError $ "Error\n" <> displayShow e

-- | Main function
main :: IO ()
main = do
    -- Parse command line
    (Options verbose level, url, auth, cmd') <- execParser optionsInfo
    -- Init the client
    let settings = case baseUrlScheme url of
                       Http -> defaultManagerSettings
                       Https -> tlsManagerSettings
    manager <- newManager settings
    let clientEnv = mkClientEnv manager url
        api = mkProtectedApi clientEnv auth
    -- Setup log options and run the command
    logOptions <- setLogMinLevel level <$> logOptionsHandle stderr verbose
    withLogFunc logOptions $ \lf -> do
        pc <- mkDefaultProcessContext
        -- Initialize the application
        let app = App { appLogFunc = lf
                      , appProcessContext = pc }
        runRIO app $ catch (run api cmd') handleError

run :: (HasLogFunc env, HasProcessContext env) => ProtectedClient env -> Cmd -> RIO env ()

run ProtectedClient{..} Migrate = void migrateAll

run ProtectedClient{..} ProjList =
    projList >>= mapM_ (logInfo . display)

run ProtectedClient{..} (ProjAdd project) = void $ projAdd project

run ProtectedClient{..} (ProjRm project) = void $ projRm project

run ProtectedClient{..} (ProjRename p1 p2) =
    void $ projRename $ MkRenameArgs p1 p2

run ProtectedClient{..} (DiaryDisplay cd tid) =
    hdGet cd tid >>= logInfo . displayHdWithDate

run ProtectedClient{..} (DiaryWeek cw) =
    weekGet cw >>= logInfo . display

run ProtectedClient{..} (DiaryWork cd tid wopts) =
    void $ hdSetWork cd tid wopts

run ProtectedClient{..} (DiaryHoliday cd tid hdt) =
    void $ hdSetIdleDay cd tid hdt

run ProtectedClient{..} (DiaryRm cd tid) = void $ hdRm cd tid

run api@ProtectedClient{..} (DiaryEdit cd tid) = do
    workOptions <- editorToOptions hdGet cd tid
    run api (DiaryWork cd tid workOptions)
