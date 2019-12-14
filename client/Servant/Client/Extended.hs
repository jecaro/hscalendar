module Servant.Client.Extended
    ( parse
    )
where

import           Data.Functor (($>))
import           Control.Applicative ((<|>))
import           Data.Attoparsec.Text (Parser, string)
import           Servant.Client (Scheme(..))

parse :: Parser Scheme
parse = string "http://" $> Http <|> string "https://" $> Https
