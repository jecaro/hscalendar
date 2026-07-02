{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.HTTP.Types.Status.Extended where

import Network.HTTP.Types.Status (Status (..))
import RIO

instance Display Status where
  display Status {statusCode = code, statusMessage = message} =
    "Code: "
      <> display code
      <> "\n"
      <> "Message: "
      <> displayBytesUtf8 message
