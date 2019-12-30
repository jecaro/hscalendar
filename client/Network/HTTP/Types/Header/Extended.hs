{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.HTTP.Types.Header.Extended
where

import           RIO
import qualified RIO.Text as T

import qualified Data.CaseInsensitive as CI (CI(..))
import           Data.Sequence (Seq(..))
import           Network.HTTP.Types.Header (Header)

instance Display (Seq Header) where
    display Empty = display T.empty
    display ((name, content) :<| xs)
        =  "Name: " <> displayBytesUtf8 (CI.original name) <> "\n"
        <> "Content: " <> displayBytesUtf8 content <> "\n"
        <> display xs

