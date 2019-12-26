{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

import           RIO
import qualified RIO.Text as T
import qualified RIO.ByteString.Lazy as BL

import           Control.Applicative (many)
import           Data.Attoparsec.Text as Att
    ( Parser
    , decimal
    , letter
    , digit
    , char
    )
import qualified Data.CaseInsensitive as CI (CI(..))
import           Data.Sequence (Seq(..))
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
import           Network.HTTP.Types.Status (Status(..))
import           Network.HTTP.Types.Header (Header)
import           Servant.API (NoContent, (:<|>)(..))
import           Servant.API.BasicAuth (BasicAuthData(..))
import qualified Servant.API.BasicAuth.Extended as BAE (parse)
import           Servant.Client
    ( BaseUrl(..)
    , ClientEnv
    , ClientM
    , GenResponse(..)
    , Response
    , ServantError(..)
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
import           Db.Project (Project)
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

indent :: Text -> Text
indent = T.unlines . map (T.cons '\t') . T.lines

instance Display Status where
    display Status { statusCode = code, statusMessage = message }
        =  "Code: " <> display code <> "\n"
        <> "Message: " <> displayBytesUtf8 message

instance Display (Seq Header) where
    display Empty = display T.empty
    display ((name, content) :<| xs)
        =  "Name: " <> displayBytesUtf8 (CI.original name) <> "\n"
        <> "Content: " <> displayBytesUtf8 content <> "\n"
        <> display xs

instance Display Response where
    display Response
        { responseStatusCode = code
        , responseHeaders = headers
        , responseHttpVersion = version
        , responseBody = body }
            =  "Status: \n" <> display (indent $ textDisplay code)
            <> "Headers: \n" <> display (indent $ textDisplay headers)
            <> "Version: " <> displayShow version <> "\n"
            <> "Body: " <> displayBytesUtf8 (BL.toStrict body)

handleError :: (HasLogFunc env) => ServantError -> RIO env ()
handleError (FailureResponse response) = logError $
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
    manager <- newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager url
        api = mkProtectedApi clientEnv auth
    -- Setup log options and run the command
    logOptions <- setLogMinLevel level <$> logOptionsHandle stderr verbose
    withLogFunc logOptions $ \lf -> runRIO lf $
        catch (run api cmd') handleError

run :: HasLogFunc env => ProtectedClient env -> Cmd -> RIO env ()

run ProtectedClient{..} Migrate = void migrate

run ProtectedClient{..} ProjList =
    projectAll >>= mapM_ (logInfo . display)

run ProtectedClient{..} (ProjAdd project) = void $ projectAdd project

run ProtectedClient{..} (ProjRm project) = void $ projectRm project

run ProtectedClient{..} (ProjRename p1 p2) =
    void $ projectRename $ MkRenameArgs p1 p2

run ProtectedClient{..} (DiaryDisplay cd tid) =
    diaryDisplay cd tid >>= logInfo . display

run ProtectedClient{..} (DiaryWork cd tid wopts) =
    void $ diarySetWork cd tid wopts

run ProtectedClient{..} (DiaryHoliday cd tid hdt) =
    void $ diarySetIdleDay cd tid hdt

run ProtectedClient{..} (DiaryRm cd tid) = void $ diaryRm cd tid

run _ _ = logInfo "Not implemented yet"
