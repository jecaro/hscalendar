-- | Fonctions related to the "Servant.API" module
module Servant.API.Extended (runAtto)
where

import           RIO
import qualified RIO.Text as Text (pack)

import           Data.Attoparsec.Text
    ( Parser
    , parseOnly
    , endOfInput
    )

-- | Taken from "Web.Internal.HttpApiData"
runAtto :: Parser a -> Text -> Either Text a
runAtto p t = case parseOnly (p <* endOfInput) t of
    Left err -> Left (Text.pack err)
    Right x  -> Right x

