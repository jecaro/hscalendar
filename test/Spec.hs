import           RIO

import           Database.Persist.Sqlite
    ( deleteWhere
    , Filter
    , runMigration
    , runSqlPersistM
    , SqlBackend
    , SqlPersistT
    , withSqliteConn
    , runSqlPersistM
    , SqlPersistM
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
import           Test.QuickCheck
    ( property
    , Property
    , arbitrary
    , ioProperty
    )
import qualified Test.QuickCheck.Monadic as Q (assert, pick, monadic, monadicIO, run, PropertyM(..))
import           Test.QuickCheck.Instances.Text()

import           Model
import           ModelFcts
    ( ModelException(..)
    , projAdd
    , projExists
    , projList
    , projRename
    , projRm
    )

runDB :: SqlBackend -> SqlPersistM a -> IO a
runDB =  flip runSqlPersistM 

migrate :: SqlBackend -> IO ()
migrate conn = runDB conn $ runMigration migrateAll

cleanProjects :: SqlBackend -> IO ()
cleanProjects conn = runDB conn $ deleteWhere ([] :: [Filter Project])

modelException :: Selector ModelException
modelException = const True

projAddProjExists :: SqlBackend -> Project -> Property
projAddProjExists conn project = Q.monadicIO $ do
    exists <- Q.run $ runDB conn (projAdd project >> projExists project)
    Q.assert exists
    noExists <- Q.run $ runDB conn (projRm project >> projExists project)
    Q.assert (not noExists)

projAddProjAdd :: SqlBackend -> Project -> Property
projAddProjAdd conn project = Q.monadicIO $ do
    exceptionRaised <- Q.run $ runDB conn $ do
        projAdd project
        catch (projAdd project >> return False) (\(ModelException _) -> return True)
    Q.assert exceptionRaised
    Q.run $ runDB conn $ projRm project

prop_projAdd :: (MonadIO m) => Q.PropertyM (SqlPersistT m) ()
prop_projAdd = do
    projName <- Q.pick arbitrary
    let project = Project projName
    Q.run $ projAdd project
    exists <- Q.run $ projExists project
    Q.assert exists
    Q.run $ projRm project

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
        context "Test properties" $ do
            it "projAdd projExists" $
                property (projAddProjExists conn)
            it "projAdd projAdd" $
                property (projAddProjAdd conn)
            it "other prop" $
                property (Q.monadic (ioProperty . runDB conn) prop_projAdd)
  where project1 = Project "TestProject1"
        project2 = Project "TestProject2"

main :: IO ()
main = runNoLoggingT . withSqliteConn ":memory:" $ \conn -> liftIO $ do
    liftIO $ hspec $ beforeAll (migrate conn) $ 
        testProjAPI conn

