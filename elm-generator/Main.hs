{-# OPTIONS_GHC -fno-warn-orphans #-}

import           RIO
import qualified RIO.Text as Text (intercalate, pack, unpack)
import qualified RIO.Time as Time (Day(..), TimeOfDay(..))

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
import           Db.HalfDay (HalfDay)
import           Db.Idle (Idle)
import           Db.IdleDayType (IdleDayType)
import           Db.Notes (Notes)
import           Db.Office (Office)
import           Db.Project (Project)
import           Db.TimeInDay (TimeInDay)
import           Db.Worked (Worked)

--

instance SOP.Generic TimeInDay
instance SOP.HasDatatypeInfo TimeInDay

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

instance SOP.Generic Idle
instance SOP.HasDatatypeInfo Idle

instance HasElmType Idle where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Idle defaultOptions "Api.Idle"

instance HasElmDecoder A.Value Idle where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @Idle defaultOptions A.defaultOptions "Api.Idle.decoder"

instance HasElmEncoder A.Value Idle where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @Idle defaultOptions A.defaultOptions "Api.Idle.encoder"

--

instance SOP.Generic Office
instance SOP.HasDatatypeInfo Office

instance HasElmType Office where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Office defaultOptions "Api.Office"

instance HasElmDecoder A.Value Office where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @Office defaultOptions A.defaultOptions "Api.Office.decoder"

instance HasElmEncoder A.Value Office where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @Office defaultOptions A.defaultOptions "Api.Office.encoder"

--

instance SOP.Generic Notes
instance SOP.HasDatatypeInfo Notes

instance HasElmType Notes where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Notes defaultOptions "Api.Notes"

instance HasElmDecoder A.Value Notes where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @Notes defaultOptions A.defaultOptions "Api.Notes.decoder"

instance HasElmEncoder A.Value Notes where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @Notes defaultOptions A.defaultOptions "Api.Notes.encoder"

--

instance SOP.Generic Project
instance SOP.HasDatatypeInfo Project

instance HasElmType Project where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Project defaultOptions "Api.Project"

instance HasElmDecoder A.Value Project where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @Project defaultOptions A.defaultOptions "Api.Project.decoder"

instance HasElmEncoder A.Value Project where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @Project defaultOptions A.defaultOptions "Api.Project.encoder"

--

instance SOP.Generic Worked
instance SOP.HasDatatypeInfo Worked

instance HasElmType Worked where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Worked defaultOptions "Api.Worked"

instance HasElmDecoder A.Value Worked where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @Worked defaultOptions A.defaultOptions "Api.Worked.decoder"

instance HasElmEncoder A.Value Worked where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @Worked defaultOptions A.defaultOptions "Api.Worked.encoder"

--

instance SOP.Generic HalfDay
instance SOP.HasDatatypeInfo HalfDay

instance HasElmType HalfDay where
    elmDefinition =
        Just $ deriveElmTypeDefinition @HalfDay defaultOptions "Api.HalfDay"

instance HasElmDecoder A.Value HalfDay where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @HalfDay defaultOptions A.defaultOptions "Api.HalfDay.decoder"

instance HasElmEncoder A.Value HalfDay where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @HalfDay defaultOptions A.defaultOptions "Api.HalfDay.encoder"

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

deriving instance Generic Time.TimeOfDay
instance SOP.Generic Time.TimeOfDay
instance SOP.HasDatatypeInfo Time.TimeOfDay

instance HasElmType Time.TimeOfDay where
    elmType = "Time.Date"

instance HasElmEncoder A.Value Time.TimeOfDay where
  elmEncoder = "Date.Extra.encode"

instance HasElmDecoder A.Value Time.TimeOfDay where
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
          [ jsonDefinitions @HalfDay
          , jsonDefinitions @Idle
          , jsonDefinitions @IdleDayType
          , jsonDefinitions @Notes
          , jsonDefinitions @Office
          , jsonDefinitions @Project
          , jsonDefinitions @Time.Day
          , jsonDefinitions @Time.TimeOfDay
          , jsonDefinitions @TimeInDay
          , jsonDefinitions @Worked
          ]
