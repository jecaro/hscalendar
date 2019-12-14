module Servant.API.BasicAuth.Extended
    ( parse
    )
where

import           RIO
import qualified RIO.Text as Text

import           Control.Applicative (many)
import           Data.Attoparsec.Text (Parser, char, inClass, satisfy)
import           Data.Char (isPrint)
import           Servant.API.BasicAuth (BasicAuthData(..))

parse :: Parser BasicAuthData
parse = do
    login <- many (satisfy $ inClass "0-9a-zA-Z_")
    _ <- char ':'
    password <- many (satisfy isPrint)
    return $ BasicAuthData
        (encodeUtf8 $ Text.pack login) (encodeUtf8 $ Text.pack password)


