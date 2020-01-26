{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Client.Extended
    ( parse
    )
where

import           RIO
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Text.Extended as T (indent)

import           Data.Functor (($>))
import           Control.Applicative ((<|>))
import           Data.Attoparsec.Text (Parser, string)
import           Network.HTTP.Types.Header.Extended ()
import           Network.HTTP.Types.Status.Extended ()
import           Servant.Client (Response, ResponseF(..), Scheme(..))

instance Display Response where
    display Response
        { responseStatusCode = code
        , responseHeaders = headers
        , responseHttpVersion = version
        , responseBody = body }
            =  "Status: \n" <> display (T.indent $ textDisplay code)
            <> "Headers: \n" <> display (T.indent $ textDisplay headers)
            <> "Version: " <> displayShow version <> "\n"
            <> "Body: " <> displayBytesUtf8 (BL.toStrict body)

parse :: Parser Scheme
parse = string "http://" $> Http <|> string "https://" $> Https

