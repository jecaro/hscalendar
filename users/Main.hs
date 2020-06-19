-- | Simple command line tool to manage users. It gives access to all the
-- common commands found in CRUD API
module Main where

import App.App (HasConnPool, initAppAndRun, runDB)
import App.CommandLine (Options (..), attoReadM, options)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Either.Combinators (whenLeft)
import Data.WorldPeace (Contains, OpenUnion)
import Database.Persist.Sql (SqlPersistM)
import qualified Db.Login as Login (Login, parser, unLogin)
import Db.Model
    ( UserExists (..),
      UserNotFound (..),
      renderUserError,
      userAdd,
      userChangePassword,
      userCheck,
      userList,
      userRename,
      userRm,
    )
import qualified Db.Password as Password (Password, parser)
import Options.Applicative
    ( (<**>),
      Parser,
      ParserInfo,
      ReadM,
      argument,
      auto,
      command,
      execParser,
      helper,
      idm,
      info,
      metavar,
      progDesc,
      subparser,
      value,
    )
import RIO

-- | Basic commands to update the user list
data UserCmd
    = UserList
    | UserRm Login.Login
    | UserAdd Login.Login Password.Password Int
    | UserRename Login.Login Login.Login
    | UserChangePassword Login.Login Password.Password Int
    | UserCheck Login.Login Password.Password

defaultCost :: Int
defaultCost = 12

readLogin :: ReadM Login.Login
readLogin = attoReadM Login.parser

readPassword :: ReadM Password.Password
readPassword = attoReadM Password.parser

userRmCmd :: Parser UserCmd
userRmCmd = UserRm <$> argument readLogin (metavar "LOGIN...")

userAddCmd :: Parser UserCmd
userAddCmd =
    UserAdd
        <$> argument readLogin (metavar "LOGIN...")
        <*> argument readPassword (metavar "PASSWORD...")
        <*> argument auto (metavar "COST..." <> value defaultCost)

userRenameCmd :: Parser UserCmd
userRenameCmd =
    UserRename
        <$> argument readLogin (metavar "LOGIN...")
        <*> argument readLogin (metavar "LOGIN...")

userChangePasswordCmd :: Parser UserCmd
userChangePasswordCmd =
    UserChangePassword
        <$> argument readLogin (metavar "LOGIN...")
        <*> argument readPassword (metavar "PASSWORD...")
        <*> argument auto (metavar "COST..." <> value defaultCost)

userCheckCmd :: Parser UserCmd
userCheckCmd =
    UserCheck
        <$> argument readLogin (metavar "LOGIN...")
        <*> argument readPassword (metavar "PASSWORD...")

userCmd :: Parser UserCmd
userCmd =
    subparser
        ( command "list" (info (pure UserList) (progDesc "List users"))
              <> command "add" (info userAddCmd (progDesc "Add a new user"))
              <> command "rm" (info userRmCmd (progDesc "Remove an existing user"))
              <> command "rename" (info userRenameCmd (progDesc "Rename a user"))
              <> command "password" (info userChangePasswordCmd (progDesc "Change a password"))
              <> command "check" (info userCheckCmd (progDesc "Check the password for a user"))
        )

optionsAndCmd :: Parser (Options, UserCmd)
optionsAndCmd = curry id <$> options <*> userCmd

optionsInfo :: ParserInfo (Options, UserCmd)
optionsInfo = info (optionsAndCmd <**> helper) idm

logUserError ::
    (HasLogFunc env, Contains err '[UserNotFound, UserExists]) =>
    OpenUnion err ->
    RIO env ()
logUserError = logError . display . renderUserError

runAndLogError ::
    (Contains err '[UserNotFound, UserExists], HasConnPool env, HasLogFunc env) =>
    ExceptT (OpenUnion err) SqlPersistM a ->
    RIO env ()
runAndLogError a = do
    r <- runDB $ runExceptT a
    whenLeft r logUserError

run :: (HasLogFunc env, HasConnPool env) => UserCmd -> RIO env ()

-- | Print the users present in the database
run UserList = runDB userList >>= mapM_ (logInfo . display . Login.unLogin)
run (UserRm login) = runAndLogError $ userRm login
run (UserAdd login password cost) =
    runAndLogError $ userAdd login password cost
run (UserRename login1 login2) = runAndLogError $ userRename login1 login2
run (UserChangePassword login password cost) =
    runAndLogError $ userChangePassword login password cost
run (UserCheck login password) =
    runDB (runExceptT (userCheck login password))
        >>= \case
            Right True -> logInfo "OK"
            Right False -> logInfo "NOK"
            Left err -> logUserError err

-- | Main function
main :: IO ()
main = do
    -- Parse command line
    (Options verbose level, cmd) <- execParser optionsInfo
    initAppAndRun verbose level (run cmd)
