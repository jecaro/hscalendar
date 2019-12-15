{-# LANGUAGE RecordWildCards #-}

import           RIO

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
import           Servant.API (NoContent, (:<|>)(..))
import           Servant.API.BasicAuth (BasicAuthData(..))
import qualified Servant.API.BasicAuth.Extended as BAE (parse)
import           Servant.Client
    ( BaseUrl(..)
    , ClientEnv
    , ClientM
    , client
    , hoistClient
    , mkClientEnv
    , runClientM
    )
import qualified Servant.Client.Extended as CE (parse)

import           App.API (protectedHSCalendarApi, RenameArgs(..))
import           App.CustomDay (CustomDay)
import           App.CommandLine
    ( Cmd(..)
    , Options(..)
    , attoReadM
    , cmd
    , options
    )
import           App.WorkOption (WorkOption)
import           Db.HalfDay (HalfDay)
import           Db.IdleDayType (IdleDayType)
import           Db.Project (Project, unProject)
import           Db.TimeInDay (TimeInDay)


-- | The main API, available once authentication is passed
data ProtectedClient env = ProtectedClient
    { migrate :: RIO env NoContent
    , projectAll :: RIO env [Project]
    , projectAdd :: Project -> RIO env NoContent
    , projectRm :: Project -> RIO env NoContent
    , projectRename :: RenameArgs -> RIO env NoContent
    , diaryDisplay :: CustomDay -> TimeInDay -> RIO env HalfDay
    , diarySetIdleDay :: CustomDay -> TimeInDay -> IdleDayType -> RIO env NoContent
    , diarySetWork :: CustomDay -> TimeInDay -> [WorkOption] -> RIO env NoContent
    , diaryRm :: CustomDay -> TimeInDay -> RIO env NoContent
    }

-- | Init the API with the client env and auth data
mkProtectedApi :: ClientEnv -> BasicAuthData -> ProtectedClient a
mkProtectedApi env ad =
  let migrate
        :<|> projectAll
        :<|> projectAdd
        :<|> projectRm
        :<|> projectRename
        :<|> diaryDisplay
        :<|> diarySetIdleDay
        :<|> diarySetWork
        :<|> diaryRm
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
    host <- many (letter <|> digit <|> char '.')
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

-- | Main function
main :: IO ()
main = do
    -- Parse command line
    (Options verbose level, url, auth, cmd') <- execParser optionsInfo
    -- Init the client
    manager <- newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager url
        api = mkProtectedApi clientEnv auth
    -- Setup log options and run the command
    logOptions <- setLogMinLevel level <$> logOptionsHandle stderr verbose
    withLogFunc logOptions $ \lf -> runRIO lf $ run api cmd'

run :: HasLogFunc env => ProtectedClient env -> Cmd -> RIO env ()

run ProtectedClient{..} Migrate = void migrate

run ProtectedClient{..} ProjList =
    projectAll >>= mapM_ (logInfo . display . unProject)

run ProtectedClient{..} (ProjAdd project) = void $ projectAdd project

run ProtectedClient{..} (ProjRm project) = void $ projectRm project

run ProtectedClient{..} (ProjRename p1 p2) =
    void $ projectRename $ MkRenameArgs p1 p2

run ProtectedClient{..} (DiaryDisplay cd tid) =
    diaryDisplay cd tid >>= logInfo . displayShow

run ProtectedClient{..} (DiaryRm cd tid) = void $ diaryRm cd tid

run _ _ = logInfo "Not implemented yet"
