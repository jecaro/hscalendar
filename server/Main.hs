{-# OPTIONS_GHC -fno-warn-orphans #-}
import           RIO
import qualified RIO.Text                 as Text
import qualified RIO.Time                 as Time (toGregorian)

import           Control.Monad.Except     (ExceptT(..))
import           Data.Attoparsec.Text
    ( Parser
    , parseOnly
    , endOfInput
    )
import           Data.Aeson (FromJSON, ToJSON)
import           Data.ByteString.Lazy.Char8 as DBLC (pack)
import           Formatting.Extended (formatTwoDigitsPadZero)
import           Network.Wai.Handler.Warp (run)
import           Servant.API              
    ( Get
    , Capture
    , DeleteNoContent
    , JSON
    , NoContent(..)
    , PostNoContent
    , PutNoContent
    , ReqBody
    , Summary
    , FromHttpApiData(..)
    , ToHttpApiData(..)
    , (:>)
    , (:<|>)(..))
import           Servant.Server          
    ( Application
    , ServantErr(..)
    , Server
    , ServerT
    , err404
    , err409
    , err409
    , hoistServer
    , serve
    )
import qualified Servant.Server as Server (Handler(..))         

import           App.App (App, HasConfig, HasConnPool, initAppAndRun, runDB)
import qualified App.CustomDay as CD (CustomDay(..), parser, toDay)
import           App.WorkOption 
    ( ProjCmdIsMandatory(..)
    , runWorkOptions
    , WorkOption(..)
    )
import           Db.HalfDay (HalfDay(..))
import           Db.IdleDayType (IdleDayType(..))
import           Db.Model 
    ( HdNotFound(..)
    , ProjExists(..)
    , ProjHasHd(..)
    , ProjNotFound(..)
    , hdGet
    , hdRm
    , hdSetHoliday
    , projAdd
    , projList
    , projRename
    , projRm
    , TimesAreWrong(..)
    )
import           Db.Project (Project)
import qualified Db.TimeInDay as TID (TimeInDay(..), parser)


data RenameArgs = MkRenameArgs { from :: Project, to :: Project }
    deriving (Eq, Generic, Show, Ord)

instance FromJSON RenameArgs
instance ToJSON RenameArgs

instance FromHttpApiData CD.CustomDay where
    parseQueryParam = runAtto CD.parser

instance ToHttpApiData CD.CustomDay where
    toQueryParam (CD.MkDay day) = Text.intercalate "-" (fmap formatTwoDigitsPadZero [d, m, intY]) 
        where (y, m, d) = Time.toGregorian day
              intY = fromIntegral y
    toQueryParam (CD.MkDayNum d)        = formatTwoDigitsPadZero d        
    toQueryParam (CD.MkDayMonthNum d m) = Text.intercalate "-" (fmap formatTwoDigitsPadZero [d, m]) 
    toQueryParam CD.Today               = "today"   
    toQueryParam CD.Yesterday           = "yesterday"   
    toQueryParam CD.Tomorrow            = "tomorrow"

instance FromHttpApiData TID.TimeInDay where
    parseQueryParam = runAtto TID.parser 

instance ToHttpApiData TID.TimeInDay where
    toQueryParam TID.Morning   = "morning"
    toQueryParam TID.Afternoon = "afternoon"

type HSCalendarApi =
        Summary "List all projects"
           :> "project"
           :> Get '[JSON] [Project]
   :<|> Summary "Add a project"
           :> "project"
           :> ReqBody '[JSON] Project
           :> PostNoContent '[JSON] NoContent
   :<|> Summary "Delete a project"
           :> "project"
           :> ReqBody '[JSON] Project
           :> DeleteNoContent '[JSON] NoContent
   :<|> Summary "Rename a project"
           :> "project"
           :> ReqBody '[JSON] RenameArgs
           :> PutNoContent '[JSON] NoContent
   :<|> Summary "Display a half-day"
           :> "diary"
           :> Capture "day" CD.CustomDay
           :> Capture "time in day" TID.TimeInDay
           :> Get '[JSON] HalfDay
   :<|> Summary "Set a non-working half-day"
           :> "diary"
           :> "idle"
           :> Capture "day" CD.CustomDay
           :> Capture "time in day" TID.TimeInDay
           :> ReqBody '[JSON] IdleDayType
           :> PutNoContent '[JSON] NoContent
   :<|> Summary "Set a working half-day"
           :> "diary"
           :> Capture "day" CD.CustomDay
           :> Capture "time in day" TID.TimeInDay
           :> ReqBody '[JSON] [WorkOption]
           :> PutNoContent '[JSON] NoContent
   :<|> Summary "Delete a half-day"
           :> "diary"
           :> Capture "day" CD.CustomDay
           :> Capture "time in day" TID.TimeInDay
           :> DeleteNoContent '[JSON] NoContent

-- | Taken from Web.Internal.HttpApiData
runAtto :: Parser a -> Text -> Either Text a
runAtto p t = case parseOnly (p <* endOfInput) t of
    Left err -> Left (Text.pack err)
    Right x  -> Right x

rioServer :: ServerT HSCalendarApi (RIO App)
rioServer =    projectAll
          :<|> projectAdd 
          :<|> projectRm
          :<|> projectRename
          :<|> diaryDisplay
          :<|> diarySetIdleDay
          :<|> diarySetWork
          :<|> diaryRm

hscalendarApi :: Proxy HSCalendarApi
hscalendarApi = Proxy

mainServer :: App -> Server HSCalendarApi
mainServer app = hoistServer hscalendarApi (nt app) rioServer

-- | https://www.parsonsmatt.org/2017/06/21/exceptional_servant_handling.html
nt :: App -> RIO App a -> Server.Handler a
nt app actions = Server.Handler . ExceptT . try $ runRIO app actions

server :: App -> Application
server app = serve hscalendarApi (mainServer app)

projectAll :: HasConnPool env => RIO env [Project]
projectAll = runDB projList 

projectRm :: HasConnPool env => Project -> RIO env NoContent
projectRm project = catches (runDB (projRm project) >> return NoContent) 
        [ Handler (\e@(ProjHasHd _)  -> throwM err409 { errBody = DBLC.pack $ show e } )
        , Handler (\(ProjNotFound _) -> throwM err404)
        ]

projectAdd :: HasConnPool env => Project -> RIO env NoContent
projectAdd project = catch (runDB (projAdd project) >> return NoContent) 
        (\e@(ProjExists _) -> throwM err409 { errBody = DBLC.pack $ show e } )

projectRename :: HasConnPool env => RenameArgs -> RIO env NoContent
projectRename (MkRenameArgs p1 p2) = catches (runDB $ projRename p1 p2 >> return NoContent)
    [ Handler (\e@(ProjExists _) -> throwM err409 { errBody = DBLC.pack $ show e } )
    , Handler (\(ProjNotFound _) -> throwM err404)
    ]

diaryDisplay :: HasConnPool env => CD.CustomDay -> TID.TimeInDay -> RIO env HalfDay
diaryDisplay cd tid = do
    -- Get actual day
    day <- CD.toDay cd
    -- Get half-day
    try (runDB $ hdGet day tid) >>=
        \case
            Left (HdNotFound _ _) -> throwM err404
            Right hd -> return hd

diarySetIdleDay 
    :: HasConnPool env 
    => CD.CustomDay -> TID.TimeInDay -> IdleDayType -> RIO env NoContent
diarySetIdleDay cd tid idt = do
    day <- CD.toDay cd
    runDB $ hdSetHoliday day tid idt
    return NoContent

diaryRm :: HasConnPool env => CD.CustomDay -> TID.TimeInDay  -> RIO env NoContent
diaryRm cd tid = do
    day <- CD.toDay cd
    catch (runDB (hdRm day tid) >> return NoContent) 
        (\(HdNotFound _ _) -> throwM err404)

diarySetWork 
    :: (HasConnPool env, HasConfig env)
    => CD.CustomDay -> TID.TimeInDay -> [WorkOption] -> RIO env NoContent
diarySetWork cd tid wopts = do
    day <- CD.toDay cd
    -- Create the record in DB
    catches (runWorkOptions day tid wopts >> return NoContent)
        [ Handler (\e@TimesAreWrong      -> throwM err409 { errBody = DBLC.pack $ show e } )
        , Handler (\e@ProjCmdIsMandatory -> throwM err409 { errBody = DBLC.pack $ show e } )
        , Handler (\(ProjNotFound _)     -> throwM err404)
        ]

main :: IO ()
main = initAppAndRun False LevelInfo $ do
        app <- ask
        liftIO . run 8081 $ server app
