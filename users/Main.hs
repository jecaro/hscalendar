-- | Simple command line tool to manage users. It gives access to all the
-- common commands found in CRUD API
import           RIO

import           Options.Applicative
    ( Parser
    , ParserInfo
    , ReadM
    , argument
    , command
    , execParser
    , helper
    , metavar
    , info
    , idm
    , progDesc
    , subparser
    , (<**>)
    )

import           App.App (HasConnPool, initAppAndRun, logException, runDB)
import           App.CommandLine (Options(..), attoReadM, options)

import qualified Db.Login as Login (Login, parser, unLogin)
import           Db.Model
    ( UserExists(..)
    , UserNotFound(..)
    , userAdd
    , userChangePassword
    , userCheck
    , userList
    , userRename
    , userRm
    )
import qualified Db.Password as Password (Password, parser)

-- | Basic commands to update the user list
data Cmd
    = UserList
    | UserRm Login.Login
    | UserAdd Login.Login Password.Password
    | UserRename Login.Login Login.Login
    | UserChangePassword Login.Login Password.Password
    | UserCheck Login.Login Password.Password

readLogin :: ReadM Login.Login
readLogin = attoReadM Login.parser

readPassword :: ReadM Password.Password
readPassword = attoReadM Password.parser

userRmCmd :: Parser Cmd
userRmCmd = UserRm <$> argument readLogin (metavar "LOGIN...")

userAddCmd :: Parser Cmd
userAddCmd = UserAdd
    <$> argument readLogin (metavar "LOGIN...")
    <*> argument readPassword (metavar "PASSWORD...")

userRenameCmd :: Parser Cmd
userRenameCmd = UserRename
    <$> argument readLogin (metavar "LOGIN...")
    <*> argument readLogin (metavar "LOGIN...")

userChangePasswordCmd :: Parser Cmd
userChangePasswordCmd = UserChangePassword
    <$> argument readLogin (metavar "LOGIN...")
    <*> argument readPassword (metavar "PASSWORD...")

userCheckCmd :: Parser Cmd
userCheckCmd = UserCheck
    <$> argument readLogin (metavar "LOGIN...")
    <*> argument readPassword (metavar "PASSWORD...")

userCmd :: Parser Cmd
userCmd = subparser
    (  command "list" (info (pure UserList) (progDesc "List users"))
    <> command "add" (info userAddCmd (progDesc "Add a new user"))
    <> command "rm" (info userRmCmd (progDesc "Remove an existing user"))
    <> command "rename" (info userRenameCmd (progDesc "Rename a user"))
    <> command "password" (info userChangePasswordCmd (progDesc "Change a password"))
    <> command "check" (info userCheckCmd (progDesc "Check the password for a user"))
    )

optionsAndCmd :: Parser (Options, Cmd)
optionsAndCmd = curry id <$> options <*> userCmd

optionsInfo :: ParserInfo (Options, Cmd)
optionsInfo = info (optionsAndCmd <**> helper) idm

run :: (HasLogFunc env, HasConnPool env) => Cmd -> RIO env ()

-- | Print the users present in the database
run UserList = runDB userList >>= mapM_ (logInfo . display . Login.unLogin)

-- | Remove a user
run (UserRm login) = catch (runDB $ userRm login)
    (\e@(UserNotFound _) -> logException e)

-- | Add a new user
run (UserAdd login password) = catch (void $ runDB $ userAdd login password)
    (\e@(UserExists _) -> logException e)

-- | Change the login of a user
run (UserRename login1 login2) = catches (runDB $ userRename login1 login2)
    [ Handler (\e@(UserExists _)   -> logException e)
    , Handler (\e@(UserNotFound _) -> logException e)
    ]

-- | Change the password of a user
run (UserChangePassword login password) =
    catch (runDB $ userChangePassword login password)
        (\e@(UserExists _) -> logException e)

-- | Validate a password for a user
run (UserCheck login password) =
    catch (do
        res <- runDB $ userCheck login password
        logInfo $ if res then "OK" else "NOK"
        )
        (\e@(UserExists _) -> logException e)

-- | Main function
main :: IO ()
main = do
    -- Parse command line
    (Options verbose level, cmd) <- execParser optionsInfo
    initAppAndRun verbose level (run cmd)
