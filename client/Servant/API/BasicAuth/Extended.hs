module Servant.API.BasicAuth.Extended
    ( parse
    )
where

import           RIO
import qualified RIO.Text as Text

import           Control.Applicative (many, (<|>))
import           Data.Attoparsec.Text (Parser, letter, digit, char)
import           Servant.API.BasicAuth (BasicAuthData(..))

parse :: Parser BasicAuthData
parse = do
    login <- many (letter <|> digit)
    _ <- char ':'
    password <- many (letter <|> digit)
    return $ BasicAuthData
        (encodeUtf8 $ Text.pack login) (encodeUtf8 $ Text.pack password)


