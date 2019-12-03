{-# LANGUAGE RecordWildCards #-}

import           RIO

import           Options.Applicative (execParser)
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Servant.API (NoContent, (:<|>)(..))
import           Servant.API.BasicAuth (BasicAuthData(..))
import           Servant.Client
    ( BaseUrl(..)
    , ClientEnv
    , ClientM
    , client
    , hoistClient
    , Scheme(Http)
    , mkClientEnv
    , runClientM
    )

import           App.API (protectedHSCalendarApi, RenameArgs)
import           App.App (initAppAndRun)
import           App.CustomDay (CustomDay)
import           App.CommandLine (Options(..), Cmd(..), opts)
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

-- | Main function
main :: IO ()
main = do
    -- Init the client
    manager <- newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager (BaseUrl Http "localhost" 8081 "")
        api = mkProtectedApi clientEnv $ BasicAuthData "jc" "jc"
    -- Parse command line
    (Options verbose level, cmd) <- execParser opts
    initAppAndRun verbose level $ run api cmd

run :: HasLogFunc env => ProtectedClient env -> Cmd -> RIO env ()
run ProtectedClient{..} ProjList = do
  res <- projectAll
  logInfo $ displayShow res
  return ()
run _ _ = logInfo "Not implemented yet"
