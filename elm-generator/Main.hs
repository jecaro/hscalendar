module Main where

import App.Api (RenameArgs)
import App.CommandLine (Options (..), options)
import App.WorkOption
    ( SetArrived,
      SetLeft,
      SetNotes,
      SetOffice,
      SetProj,
      WorkOption,
    )
import Control.Applicative (optional)
import qualified Data.HashMap.Lazy as HM
import Db.DayF (DayWithHalfDays)
import Db.HalfDay (HalfDay)
import Db.Month (Month)
import Db.MonthF (MonthWithDays)
import Db.Notes (Notes)
import Db.Off (Off)
import Db.OffDayType (OffDayType)
import Db.Office (Office)
import Db.Project (Project)
import Db.TimeInDay (TimeInDay)
import Db.Worked (Worked)
import qualified Language.Elm.Pretty as P
import qualified Language.Elm.Simplification as S
import Language.Haskell.To.Elm (jsonDefinitions)
import Options.Applicative
    ( (<**>),
      Parser,
      ParserInfo,
      execParser,
      help,
      helper,
      idm,
      info,
      long,
      metavar,
      short,
      strOption,
    )
import Orphans ()
import Path
    ( (</>),
      Abs,
      Dir,
      File,
      Path,
      parent,
      parseAbsDir,
      parseRelDir,
      parseRelFile,
      toFilePath,
    )
import Path.IO (createDirIfMissing, getCurrentDir)
import RIO
import qualified RIO.Text as Text (intercalate, pack, unpack)
import qualified RIO.Time as Time (Day (..), TimeOfDay (..))

argParser :: Parser String
argParser =
    strOption
        ( short 'o'
              <> long "out"
              <> help "Directory where to write elm modules"
              <> metavar "DIR"
        )

optionsAndArg :: Parser (Options, Maybe String)
optionsAndArg = (,) <$> options <*> optional argParser

optionsInfo :: ParserInfo (Options, Maybe String)
optionsInfo = info (optionsAndArg <**> helper) idm

-- | Convert string passed from the command line to a well typed absolute path
-- Append a relative directory to the current directory if needed
stringToPath :: String -> RIO env (Path Abs Dir)
stringToPath outDirStr@('/' : _) = parseAbsDir outDirStr
stringToPath outDirStr = do
    currentDir <- getCurrentDir
    outDir <- parseRelDir outDirStr
    pure $ currentDir </> outDir

-- | Convert an Elm module path to an absolute file path
moduleNameToFilename :: Path Abs Dir -> [Text] -> Maybe (Path Abs File)
moduleNameToFilename dir [x] = do
    xPath <- parseRelFile $ Text.unpack x <> ".elm"
    pure $ dir </> xPath
moduleNameToFilename dir (x : xs) = do
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
        modules =
            HM.toList $ P.modules $ fmap S.simplifyDefinition $
                mconcat
                    [ jsonDefinitions @DayWithHalfDays,
                      jsonDefinitions @HalfDay,
                      jsonDefinitions @Month,
                      jsonDefinitions @MonthWithDays,
                      jsonDefinitions @Notes,
                      jsonDefinitions @Off,
                      jsonDefinitions @OffDayType,
                      jsonDefinitions @Office,
                      jsonDefinitions @Project,
                      jsonDefinitions @RenameArgs,
                      jsonDefinitions @SetArrived,
                      jsonDefinitions @SetLeft,
                      jsonDefinitions @SetNotes,
                      jsonDefinitions @SetOffice,
                      jsonDefinitions @SetProj,
                      jsonDefinitions @Time.Day,
                      jsonDefinitions @TimeInDay,
                      jsonDefinitions @Time.TimeOfDay,
                      jsonDefinitions @Worked,
                      jsonDefinitions @WorkOption
                    ]
