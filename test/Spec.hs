{-# LANGUAGE TemplateHaskell            #-}
import           RIO
import           RIO.List as L (sort)

import           Database.Persist.Sqlite
    ( deleteWhere
    , Filter
    , runMigration
    , runSqlPersistM
    , SqlBackend
    , withSqliteConn
    , runSqlPersistM
    , SqlPersistM
    , SqlPersistT
    )
import           Control.Monad.Logger (runNoLoggingT)
import           Refined (refineTH)
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
import           Test.QuickCheck (property, Property, ioProperty)
import qualified Test.QuickCheck.Monadic as Q (assert, monadic, run)
import           Test.QuickCheck.Instances.Text()

import           ModelExports
import           ModelFcts
    ( ModelException(..)
    , projAdd
    , projExists
    , projList
    , projRename
    , projRm
    )

runDB :: SqlBackend -> SqlPersistM a -> IO a
runDB conn actions = runSqlPersistM actions conn

cleanProjects :: (MonadIO m) => SqlPersistT m ()
cleanProjects = deleteWhere ([] :: [Filter Project])

modelException :: Selector ModelException
modelException = const True

prop_projAddProjExists :: SqlBackend -> Project -> Property
prop_projAddProjExists conn project = Q.monadic (ioProperty . runDB conn) $ do
    exists <- Q.run (projAdd project >> projExists project)
    Q.assert exists -- need clean up if it crashes
    noExists <- Q.run (projRm project >> projExists project)
    Q.assert (not noExists)

prop_projAddProjAdd :: SqlBackend -> Project -> Property
prop_projAddProjAdd conn project =  Q.monadic (ioProperty . runDB conn) $ do
    exceptionRaised <- Q.run $ do
        projAdd project
        res <- catch (projAdd project >> return False) (\(ModelException _) -> return True)
        cleanProjects
        return res

    Q.assert exceptionRaised

prop_projList :: SqlBackend -> [Project] -> Property
prop_projList conn projects = Q.monadic (ioProperty . runDB conn) $ do
    let uniqueProjects = nubOrd projects
    dbProjects <- Q.run $ do
        mapM_ projAdd uniqueProjects
        res <- projList
        cleanProjects
        return res

    Q.assert $ dbProjects == sort uniqueProjects

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
            $ after_ (runDB conn cleanProjects)
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
        context "Test properties" $ do
            it "projAdd projExists" $
                property (prop_projAddProjExists conn)
            it "projAdd projAdd" $
                property (prop_projAddProjAdd conn)
            it "projList" $
                property (prop_projList conn)
  where project1 = mkProjectLit $$(refineTH "TestProject1") 
        project2 = mkProjectLit $$(refineTH "TestProject2") 

main :: IO ()
main = runNoLoggingT . withSqliteConn ":memory:" $ \conn -> liftIO $ 
    liftIO $ hspec $ beforeAll (runDB conn $ runMigration migrateAll) $ 
        testProjAPI conn

