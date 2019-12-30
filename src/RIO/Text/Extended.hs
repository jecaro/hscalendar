module RIO.Text.Extended (indent)
where

import           RIO
import qualified RIO.Text as T

-- | Indent a `Text` with a tabulation
indent :: Text -> Text
indent = T.unlines . map (T.cons '\t') . T.lines

