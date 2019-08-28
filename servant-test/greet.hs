{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

import           RIO

import           Control.Concurrent
import           Data.Aeson
import           Data.Text                (Text)
import           Network.HTTP.Client      (newManager, defaultManagerSettings)
import           Network.Wai.Handler.Warp (run)
import           Options.Applicative      (header, progDesc)
import           Servant.API
import           Servant.CLI
import           Servant.Client
import           Servant.Server
import qualified Data.Text                as T


-- * Example

-- | A greet message data type
newtype Greet = Greet Text
  deriving (Generic, Show)

instance ParseBody Greet where
    parseBody = Greet <$> parseBody

-- | We can get JSON support automatically. This will be used to parse
-- and encode a Greeting as 'JSON'.
instance FromJSON Greet
instance ToJSON Greet

-- We add some useful annotations to our captures,
-- query parameters and request body to make the docs
-- really helpful.
instance ToCapture (Capture "name" Text) where
    toCapture _ = DocCapture "name" "name of the person to greet"

instance ToParam (QueryParam "capital" Bool) where
    toParam _ =
      DocQueryParam "capital"
                    ["true", "false"]
                    "Get the greeting message in uppercase (true) or not (false). Default is false."
                    Normal

type TestApi =
        Summary "Send a greeting"
           :> "hello"
           :> Capture "name" Text
           :> QueryParam "capital" Bool
           :> Get '[JSON] Greet
   :<|> Summary "Greet utilities"
           :> "greet"
           :> ReqBody '[JSON] Greet
           :> Get '[JSON] Int
   :<|> Summary "Deep paths test"
           :> "dig"
           :> "down"
           :> "deep"
           :> Summary "Almost there"
           :> Capture "name" Text
           :> "more"
           :> Summary "We made it"
           :> Get '[JSON] Text


testApi :: Proxy TestApi
testApi = Proxy

server :: Application
server = serve testApi $
        (\t b -> pure . Greet $ "Hello, "
              <> if fromMaybe False b
                    then T.toUpper t
                    else t
        )
   :<|> (\(Greet g) -> pure (T.length g))
   :<|> (pure . T.reverse)

main :: IO ()
main = do
    _ <- forkIO $ run 8081 server

    manager <- newManager defaultManagerSettings

    runSimpleApp $ do
        c <- liftIO $ parseHandleClient
                        testApi
                        (Proxy :: Proxy ClientM)
                        (header "greet" <> progDesc "Greet API") $
                (\(Greet g) -> "Greeting: " ++ T.unpack g)
           :<|> (\i -> show i ++ " letters")
           :<|> (\s -> "Reversed: " ++ T.unpack s)

        res <- liftIO $ runClientM c (mkClientEnv manager (BaseUrl Http "localhost" 8081 ""))

        case res of
          Left e        -> throwIO e
          Right rstring -> logInfo $ displayShow rstring
