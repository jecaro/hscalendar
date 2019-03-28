import           RIO
import qualified RIO.List.Partial as L (head)

import           Database.Persist.Sqlite
    ( runMigration
    , runSqlConn
    , withSqliteConn
    , IsSqlBackend
    )
import           Control.Monad.Logger (runNoLoggingT)

import           Test.Hspec

import           Model
import           ModelFcts
    ( projAdd
    , projExists
    , )

runDB 
    :: (MonadUnliftIO m, IsSqlBackend backend) 
    => backend 
    -> ReaderT backend m a -> m a
runDB conn x = runSqlConn x conn

shouldBeIO :: (MonadIO m, Show a, Eq a) => a -> a -> m ()
shouldBeIO x y = liftIO $ x `shouldBe` y

main :: IO ()
main = runNoLoggingT $ withSqliteConn ":memory:" $ \connection ->
    liftIO $ do
        runSqlConn (runMigration migrateAll) connection
        hspec $ do
          describe "RIO.List.Partial.head" $
            it "returns the first element of a list" $
            L.head [23 ..] `shouldBe` (23 :: Int)
          describe "Project" $
            it "adds a project to the database" $ asIO $ runDB connection $ do
            let project = Project "Test"
            projAdd project
            exists <- projExists project
            exists `shouldBeIO` True

