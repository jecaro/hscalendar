{-# LANGUAGE ScopedTypeVariables #-}
import           RIO

import           Database.Persist.Sqlite
    ( deleteWhere
    , Filter
    , runMigration
    , runSqlConn
    , SqlBackend
    , withSqliteConn
    )
import           Control.Monad.Logger (runNoLoggingT)
import           Test.Hspec 
    ( after
    , before
    , beforeAll
    , describe
    , hspec
    , it
    , Selector
    , shouldBe
    , shouldReturn
    , shouldThrow
    , SpecWith
    )

import           Model
import           ModelFcts
    ( ModelException()
    , projAdd
    , projExists
    , projRename
    , projRm
    )

runDB :: MonadUnliftIO m => SqlBackend -> ReaderT SqlBackend m a -> m a
runDB conn actions = runSqlConn actions conn

migrate :: MonadUnliftIO m => SqlBackend -> m ()  
migrate conn = runDB conn $ runMigration migrateAll

cleanProjects :: MonadUnliftIO m => SqlBackend -> m ()
cleanProjects conn = runDB conn $ deleteWhere ([] :: [Filter Project])

modelException :: Selector ModelException
modelException = const True

testProject :: SpecWith SqlBackend
testProject = 
    describe "Test the project API" $ after cleanProjects $ do
        it "tests the uniqueness of the name" $ \conn -> 
            runDB conn (do
                projAdd project1 
                projAdd project1)
              `shouldThrow` modelException
        it "tests if a project does not exists" $ \conn -> 
            runDB conn (projExists project1) `shouldReturn` False
        it "tests if a project exists" $ \conn -> do
            exists <- runDB conn $ do 
                projAdd project1
                projExists project1
            exists `shouldBe` True
        it "tests if we can remove a project not present in the db" $ \conn ->
            runDB conn (projRm project1) `shouldThrow` modelException
        it "tests if we can remove a project present in the db" $ \conn -> do
            exists <- runDB conn $ do
                projAdd project1 
                projRm project1 
                projExists project1
            exists `shouldBe` False
        it "tests if we can rename a project not present in the db" $ \conn ->
            runDB conn (projRename project1 project2) `shouldThrow` modelException
        it "tests if we can rename a project present in the db" $ \conn -> do
            runDB conn $ do
                projAdd project1 
                projRename project1 project2
            proj1Exists <- runDB conn (projExists project1)
            proj1Exists `shouldBe` False
            proj2Exists <- runDB conn (projExists project2)
            proj2Exists `shouldBe` True
  where project1 = Project "TestProject1"
        project2 = Project "TestProject2"

main :: IO ()
main = runNoLoggingT . withSqliteConn ":memory:" $ \conn -> 
    liftIO $ hspec $ beforeAll (migrate conn) $ before (return conn) $ 
        testProject
        
