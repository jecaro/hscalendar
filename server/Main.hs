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
    , Delete
    , JSON
    , NoContent(..)
    , Post
    , PostNoContent
    , Put
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

import           App.App (App, HasConnPool, initAppAndRun, runDB)
import qualified App.CustomDay as CD (CustomDay(..), parser, toDay)
import           Db.HalfDay (HalfDay(..))
import           Db.IdleDayType (IdleDayType(..))
import           Db.Model 
    ( HdNotFound(..)
    , ProjExists(..)
    , ProjHasHd(..)
    , ProjNotFound(..)
    , hdGet
    , hdSetHoliday
    , projAdd
    , projList
    , projRename
    , projRm
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
           :> "all"
           :> Get '[JSON] [Project]
   :<|> Summary "Rm project"
           :> "project"
           :> "rm"
           :> ReqBody '[JSON] Project
           :> Delete '[JSON] ()
   :<|> Summary "Add a project"
           :> "project"
           :> "add"
           :> ReqBody '[JSON] Project
           :> Post '[JSON] Project
   :<|> Summary "Rename a project"
           :> "project"
           :> "rename"
           :> ReqBody '[JSON] RenameArgs
           :> Put '[JSON] Project
   :<|> Summary "Display a half-day"
           :> Capture "day" CD.CustomDay
           :> Capture "time in day" TID.TimeInDay
           :> Get '[JSON] HalfDay
   :<|> Summary "Set a non-working half-day"
           :> Capture "day" CD.CustomDay
           :> Capture "time in day" TID.TimeInDay
           :> ReqBody '[JSON] IdleDayType
           :> PostNoContent '[JSON] NoContent

-- | Taken from Web.Internal.HttpApiData
runAtto :: Parser a -> Text -> Either Text a
runAtto p t = case parseOnly (p <* endOfInput) t of
    Left err -> Left (Text.pack err)
    Right x  -> Right x

rioServer :: ServerT HSCalendarApi (RIO App)
rioServer =    allProjects 
          :<|> rmProject 
          :<|> addProject 
          :<|> renameProject
          :<|> displayHd
          :<|> setIdleDay

hscalendarApi :: Proxy HSCalendarApi
hscalendarApi = Proxy

mainServer :: App -> Server HSCalendarApi
mainServer app = hoistServer hscalendarApi (nt app) rioServer

-- | https://www.parsonsmatt.org/2017/06/21/exceptional_servant_handling.html
nt :: App -> RIO App a -> Server.Handler a
nt app actions = Server.Handler . ExceptT . try $ runRIO app actions

server :: App -> Application
server app = serve hscalendarApi (mainServer app)

allProjects :: HasConnPool env => RIO env [Project]
allProjects = runDB projList 

rmProject :: HasConnPool env => Project -> RIO env ()
rmProject project = catches (runDB (projRm project)) 
        [ Handler (\e@(ProjHasHd _)  -> throwM err409 { errBody = DBLC.pack $ show e } )
        , Handler (\(ProjNotFound _) -> throwM err404)
        ]

addProject :: HasConnPool env => Project -> RIO env Project
addProject project = catch (runDB (projAdd project) >> return project) 
        (\e@(ProjExists _) -> throwM err409 { errBody = DBLC.pack $ show e } )

renameProject :: HasConnPool env => RenameArgs -> RIO env Project
renameProject (MkRenameArgs p1 p2) = catches (runDB $ projRename p1 p2 >> return p2)
    [ Handler (\e@(ProjExists _) -> throwM err409 { errBody = DBLC.pack $ show e } )
    , Handler (\(ProjNotFound _) -> throwM err404)
    ]

displayHd :: CD.CustomDay -> TID.TimeInDay -> RIO App HalfDay
displayHd cd tid = do
    -- Get actual day
    day <- CD.toDay cd
    -- Get half-day
    try (runDB $ hdGet day tid) >>=
        \case
            Left (HdNotFound _ _) -> throwM err404
            Right hd -> return hd

setIdleDay :: CD.CustomDay -> TID.TimeInDay -> IdleDayType -> RIO App NoContent
setIdleDay cd tid idt = do
    day <- CD.toDay cd
    runDB $ hdSetHoliday day tid idt
    return NoContent

main :: IO ()
main = initAppAndRun False LevelInfo $ do
    
        app <- ask
        liftIO . run 8081 $ server app
