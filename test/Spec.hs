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
    ( after_
    , before_
    , beforeAll
    , context
    , describe
    , hspec
    , it
    , Selector
    , shouldBe
    , shouldReturn
    , shouldThrow
    , Spec
    )

import           Model
import           ModelFcts
    ( ModelException()
    , projAdd
    , projExists
    , projList
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

testProjAPI :: SqlBackend -> Spec
testProjAPI conn = 
    describe "Test the project API" $ do
        context "When the DB is empty" $ do
            it "tests the uniqueness of the name" $ 
                runDB conn (projAdd project1 >> projAdd project1)
                  `shouldThrow` modelException
            it "tests if a project does not exists" $ 
                runDB conn (projExists project1) `shouldReturn` False
            it "tests if we can remove a project" $ 
                runDB conn (projRm project1) `shouldThrow` modelException
            it "tests if we can rename a project not present in the db" $ 
                runDB conn (projRename project1 project2) 
                    `shouldThrow` modelException
            it "tests the list of projects" $ 
                runDB conn projList `shouldReturn` []
        context "One project in DB" 
            $ before_ (runDB conn (projAdd project1))
            $ after_ (cleanProjects conn)
            $ do
            it "tests if the project exists" $ 
                runDB conn (projExists project1) `shouldReturn` True
            it "tests if we can remove the project" $  do
                exists <- runDB conn $ do
                    projRm project1 
                    projExists project1
                exists `shouldBe` False
            it "tests if we can rename it" $ do
                runDB conn $ projRename project1 project2
                proj1Exists <- runDB conn (projExists project1)
                proj1Exists `shouldBe` False
                proj2Exists <- runDB conn (projExists project2)
                proj2Exists `shouldBe` True
            it "tests the list of projects" $ 
                runDB conn projList `shouldReturn` [project1]
  where project1 = Project "TestProject1"
        project2 = Project "TestProject2"

main :: IO ()
main = runNoLoggingT . withSqliteConn ":memory:" $ \conn -> 
    liftIO $ hspec $ beforeAll (migrate conn) $ 
        testProjAPI conn
        
