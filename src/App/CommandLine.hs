module App.CommandLine
    ( Cmd(..)
    , Options(..)
    , attoReadM
    , cmd
    , options
    ) where

import           RIO
import qualified RIO.Text as Text (pack)
import qualified RIO.Time as Time (TimeOfDay(..))
import qualified RIO.Time.Extended as Time (parser)

import           Data.Attoparsec.Text as Atto
    ( Parser
    , string
    , endOfInput
    , parseOnly
    )
import           Data.Functor (($>))
import           Options.Applicative as Opt
    ( Parser
    , ReadM
    , argument
    , command
    , eitherReader
    , flag'
    , help
    , hsubparser
    , info
    , long
    , metavar
    , option
    , progDesc
    , short
    , some
    , subparser
    , switch
    , value
    , (<|>)
    )

import qualified App.CustomDay as CD (CustomDay(..), parser)
import           App.WorkOption
    ( WorkOption(..)
    , SetProj(..)
    , SetNotes(..)
    , SetArrived(..)
    , SetLeft(..)
    , SetOffice(..)
    )
import qualified Db.IdleDayType as IDT (IdleDayType(..), parser)
import qualified Db.Notes as Notes (Notes, parser)
import qualified Db.Office as Office (Office(..), parser)
import qualified Db.Project as Project (Project, parser)
import           Db.TimeInDay (TimeInDay(..))

-- | Options for the log function
data Options = Options { optVerbose :: !Bool,
                         optLevel   :: !LogLevel }

-- | Commands handle by command line
data Cmd = Migrate                                             |
           DiaryDisplay CD.CustomDay TimeInDay                 |
           DiaryEdit CD.CustomDay TimeInDay                    |
           DiaryHoliday CD.CustomDay TimeInDay IDT.IdleDayType |
           DiaryRm CD.CustomDay TimeInDay                      |
           DiaryWork CD.CustomDay TimeInDay [WorkOption]       |
           ProjAdd Project.Project                             |
           ProjList                                            |
           ProjRename Project.Project Project.Project          |
           ProjRm Project.Project
    deriving (Eq, Show)

attoReadM :: Atto.Parser a -> ReadM a
attoReadM p = eitherReader (parseOnly (p <* endOfInput) . Text.pack)

readOffice :: ReadM Office.Office
readOffice = attoReadM Office.parser

readTimeOfDay :: ReadM Time.TimeOfDay
readTimeOfDay = attoReadM Time.parser

readCustomDay :: ReadM CD.CustomDay
readCustomDay = attoReadM CD.parser

readLevel :: ReadM LogLevel
readLevel = attoReadM parser
  where parser =   string "debug" $> LevelDebug
               <|> string "info"  $> LevelInfo
               <|> string "warn"  $> LevelWarn
               <|> string "error" $> LevelError

readHalfDayType :: ReadM IDT.IdleDayType
readHalfDayType = attoReadM IDT.parser

readProject :: ReadM Project.Project
readProject = attoReadM Project.parser

projRm :: Opt.Parser Cmd
projRm = ProjRm <$> argument readProject (metavar "PROJECT...")

projRename :: Opt.Parser Cmd
projRename = ProjRename
    <$> argument readProject (metavar "PROJECT...")
    <*> argument readProject (metavar "PROJECT...")

projAdd :: Opt.Parser Cmd
projAdd = ProjAdd <$> argument readProject (metavar "PROJECT...")

projCmd :: Opt.Parser Cmd
projCmd = subparser
    (  command "list"   (info (pure ProjList) (progDesc "List current projects"))
    <> command "add"    (info projAdd         (progDesc "Add project"))
    <> command "rm"     (info projRm          (progDesc "Remove project"))
    <> command "rename" (info projRename      (progDesc "Rename project"))
    )

diaryDisplay :: Opt.Parser Cmd
diaryDisplay = DiaryDisplay
    <$> argument readCustomDay (metavar "DAY")
    <*> tidOption

diaryRm :: Opt.Parser Cmd
diaryRm = DiaryRm
    <$> argument readCustomDay (metavar "DAY")
    <*> tidOption

diaryEdit :: Opt.Parser Cmd
diaryEdit = DiaryEdit
    <$> argument readCustomDay (metavar "DAY")
    <*> tidOption

diaryHoliday :: Opt.Parser Cmd
diaryHoliday = DiaryHoliday
    <$> argument readCustomDay (metavar "DAY")
    <*> tidOption
    <*> argument readHalfDayType (metavar "HALF-DAY TYPE")

diaryWork :: Opt.Parser Cmd
diaryWork = DiaryWork
    <$> argument readCustomDay (metavar "DAY")
    <*> tidOption
    <*> some workOption

readNotes :: ReadM Notes.Notes
readNotes = attoReadM Notes.parser

tidOption :: Opt.Parser TimeInDay
tidOption =   flag' Morning (long "morning" <> short 'm')
          <|> flag' Afternoon (long "afternoon" <> short 'a')

diaryCmd :: Opt.Parser Cmd
diaryCmd = hsubparser
    (  command "display" (info diaryDisplay (progDesc "Display entry"))
    <> command "work"    (info diaryWork    (progDesc "Set work entry"))
    <> command "holiday" (info diaryHoliday (progDesc "Set holiday entry"))
    <> command "rm"      (info diaryRm      (progDesc "Remove entry"))
    <> command "edit"    (info diaryEdit    (progDesc "Edit entry"))
    )

workOption :: Opt.Parser WorkOption
workOption = workOptionSetProj    <|>
             workOptionSetNotes   <|>
             workOptionSetArrived <|>
             workOptionSetLeft    <|>
             workOptionSetOffice

workOptionSetProj :: Opt.Parser WorkOption
workOptionSetProj = MkSetProj . SetProj <$> option readProject
    (  long "project"
    <> short 'p'
    <> metavar "PROJECT"
    <> help "Set the project" )

workOptionSetNotes :: Opt.Parser WorkOption
workOptionSetNotes = MkSetNotes . SetNotes <$> option readNotes
    (  long "notes"
    <> short 'n'
    <> metavar "NOTES"
    <> help "Set the notes" )

workOptionSetArrived :: Opt.Parser WorkOption
workOptionSetArrived = MkSetArrived . SetArrived <$> option readTimeOfDay
    (  long "arrived"
    <> short 'a'
    <> metavar "TIME"
    <> help "Time of arrival" )

workOptionSetLeft :: Opt.Parser WorkOption
workOptionSetLeft = MkSetLeft . SetLeft <$> option readTimeOfDay
    (  long "left"
    <> short 'l'
    <> metavar "TIME"
    <> help "Time of leaving" )

workOptionSetOffice :: Opt.Parser WorkOption
workOptionSetOffice = MkSetOffice . SetOffice <$> option readOffice
    (  long "office"
    <> short 'o'
    <> metavar "OFFICE"
    <> help "Office" )

cmd :: Opt.Parser Cmd
cmd = hsubparser
    (  command "project" (info projCmd  (progDesc "Project commands"))
    <> command "migrate" (info (pure Migrate) (progDesc "Migrate database"))
    <> command "diary"   (info diaryCmd (progDesc "Diary commands"))
    )

options :: Opt.Parser Options
options = Options
    <$> switch (long "verbose" <> short 'v' <> help "Verbose output")
    <*> option readLevel
        (  long "level"
        <> short 'l'
        <> metavar "LEVEL"
        <> help "log level"
        <> value LevelInfo
        )


