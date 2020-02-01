{-# OPTIONS_GHC -fno-warn-orphans #-}

import           RIO
import qualified RIO.Text as Text (intercalate, pack, unpack)
import qualified RIO.Time as Time (Day(..))

import           Control.Applicative (optional)
import qualified Data.Aeson as A
import qualified Data.HashMap.Lazy as HM
import qualified Generics.SOP as SOP
import qualified Language.Elm.Pretty as P
import qualified Language.Elm.Simplification as S
import           Language.Haskell.To.Elm
    ( HasElmEncoder(..)
    , HasElmDecoder(..)
    , HasElmType(..)
    , defaultOptions
    , deriveElmJSONEncoder
    , deriveElmJSONDecoder
    , deriveElmTypeDefinition
    , jsonDefinitions
    )
import           Options.Applicative
    ( Parser
    , ParserInfo
    , execParser
    , help
    , helper
    , idm
    , info
    , long
    , metavar
    , short
    , strOption
    , (<**>)
    )
import           Path
    ( Path
    , Abs
    , Dir
    , File
    , parent
    , parseAbsDir
    , parseRelDir
    , parseRelFile
    , toFilePath
    , (</>)
    )
import           Path.IO (createDirIfMissing, getCurrentDir)

import           App.CommandLine (Options(..), options)
import           Db.IdleDayType (IdleDayType)
import           Db.TimeInDay (TimeInDay)

instance         SOP.Generic TimeInDay
instance         SOP.HasDatatypeInfo TimeInDay

instance HasElmType TimeInDay where
    elmDefinition =
        Just $ deriveElmTypeDefinition @TimeInDay defaultOptions "Api.TimeInDay"

instance HasElmDecoder A.Value TimeInDay where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @TimeInDay defaultOptions A.defaultOptions "Api.TimeInDay.decoder"

instance HasElmEncoder A.Value TimeInDay where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @TimeInDay defaultOptions A.defaultOptions "Api.TimeInDay.encoder"

--

instance SOP.Generic IdleDayType
instance SOP.HasDatatypeInfo IdleDayType

instance HasElmType IdleDayType where
    elmDefinition =
        Just $ deriveElmTypeDefinition @IdleDayType defaultOptions "Api.IdleDayType"

instance HasElmDecoder A.Value IdleDayType where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @IdleDayType defaultOptions A.defaultOptions "Api.IdleDayType.decoder"

instance HasElmEncoder A.Value IdleDayType where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @IdleDayType defaultOptions A.defaultOptions "Api.IdleDayType.encoder"

--

deriving instance Generic Time.Day
instance SOP.Generic Time.Day
instance SOP.HasDatatypeInfo Time.Day

instance HasElmType Time.Day where
    elmType = "Time.Date"

instance HasElmEncoder A.Value Time.Day where
  elmEncoder = "Date.Extra.encode"

instance HasElmDecoder A.Value Time.Day where
  elmDecoder = "Date.Extra.decoder"

--

argParser :: Parser String
argParser = strOption
          ( short 'o'
         <> long "out"
         <> help "Directory where to write elm modules"
         <> metavar "DIR" )

optionsAndArg :: Parser (Options, Maybe String)
optionsAndArg = (,) <$> options <*> optional argParser

optionsInfo :: ParserInfo (Options, Maybe String)
optionsInfo = info (optionsAndArg <**> helper) idm

-- | Convert string passed from the command line to a well typed absolute path
-- Append a relative directory to the current directory if needed
stringToPath :: String -> RIO env (Path Abs Dir)
stringToPath outDirStr@('/':_) = parseAbsDir outDirStr
stringToPath outDirStr = do
    currentDir <- getCurrentDir
    outDir <- parseRelDir outDirStr
    return $ currentDir </> outDir

-- | Convert an Elm module path to an absolute file path
moduleNameToFilename :: Path Abs Dir -> [Text] -> Maybe (Path Abs File)
moduleNameToFilename dir [x] = do
    xPath <- parseRelFile $ Text.unpack x <> ".elm"
    return $ dir </> xPath
moduleNameToFilename dir (x:xs) = do
     xPath <- parseRelDir $ Text.unpack x
     moduleNameToFilename (dir </> xPath) xs
moduleNameToFilename _ [] = Nothing

main :: IO ()
main = do
  (Options verbose level, arg) <- execParser optionsInfo
  logOptions <- setLogMinLevel level <$> logOptionsHandle stderr verbose

  withLogFunc logOptions $ \lf -> runRIO lf $
      case arg of
          Just outDirStr -> do
              outDir <- stringToPath outDirStr
              forM_ modules $ \(moduleName, moduleBody) ->
                  case moduleNameToFilename outDir moduleName of
                      Nothing -> logError "Unable to construct output path"
                      Just filename -> do
                          createDirIfMissing True $ parent filename
                          logInfo $ "Write " <> displayShow filename
                          writeFileUtf8 (toFilePath filename) (Text.pack $ show moduleBody)
          Nothing -> do
              logInfo "No command-line argument given to specify the out directory; printing instead"
              forM_ modules $ \(moduleName, moduleBody) -> do
                  logInfo $ display $ Text.intercalate "." moduleName <> ":"
                  logInfo $ displayShow moduleBody
  where
    modules = HM.toList $ P.modules $ fmap S.simplifyDefinition $
        mconcat
          [ jsonDefinitions @TimeInDay
          , jsonDefinitions @IdleDayType
          ]
