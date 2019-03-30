import           RIO

import           Database.Persist.Sqlite
    ( runMigration
    , runSqlConn
    , withSqliteConn
    , SqlBackend
    )
import           Control.Monad.Logger
import           Test.Hspec

import           Model
import           ModelFcts
    ( projAdd
    , projExists
    , )

runDB :: MonadUnliftIO m => SqlBackend -> ReaderT SqlBackend m a -> m a
runDB conn x = runSqlConn x conn

testProject :: SpecWith SqlBackend
testProject =
    describe "Test the project API" $ do
        it "creates a new project" $ \conn ->
            runDB conn (projAdd project) `shouldReturn` ()
        -- it "creates a new project" $ \conn ->
        --     runDB conn (projAdd project) `shouldReturn` ()
        it "test something" $ \_ ->
            True `shouldBe` True
        it "tests if its exists" $ \conn -> do
            exists <- runDB conn (projExists project)
            exists `shouldBe` True
  where project = Project "TestProject"

main :: IO ()
main = runNoLoggingT . withSqliteConn ":memory:" $ \conn -> do
    runSqlConn (runMigration migrateAll) conn
    liftIO $ hspec $ beforeAll (runSqlConn (runMigration migrateAll) conn) $ before (return conn) $ do
        testProject
        testProject
