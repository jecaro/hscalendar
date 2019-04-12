{-# LANGUAGE TemplateHaskell #-}

import           RIO
import           RIO.List as L (sort)
import qualified RIO.Set as Set (fromList)

import           Database.Persist.Sqlite
    ( deleteWhere
    , Filter
    , runMigration
    , runSqlPersistM
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
import           Test.QuickCheck 
    ( Arbitrary(..)
    , Property
    , property
    , listOf
    , ioProperty
    , suchThat)
import qualified Test.QuickCheck.Monadic as Q (assert, monadic, run)
import           Test.QuickCheck.Instances.Text()

import           ModelExports (Project, mkProjectLit, migrateAll)
import           ModelFcts
    ( ModelException(..)
    , projAdd
    , projExists
    , projList
    , projRename
    , projRm
    )

-- The type for the runDB function
type RunDB = (forall a. SqlPersistM a -> IO a)

-- Clean up the project table
cleanProjects :: (MonadIO m) => SqlPersistT m ()
cleanProjects = deleteWhere ([] :: [Filter Project])

-- QuickCheck selector for our ModelException
modelException :: Selector ModelException
modelException = const True

prop_projAddProjExists :: RunDB -> Project -> Property
prop_projAddProjExists runDB project = Q.monadic (ioProperty . runDB) $ do
    (exists, noExists) <- Q.run $ do
        projAdd project 
        exists <- projExists project
        projRm project 
        noExists <- projExists project
        cleanProjects
        return (exists, noExists)

    Q.assert (exists && not noExists)

prop_projAddProjAdd :: RunDB -> Project -> Property
prop_projAddProjAdd runDB project =  Q.monadic (ioProperty . runDB) $ do
    exceptionRaised <- Q.run $ do
        projAdd project
        res <- catch (projAdd project >> return False) (\(ModelException _) -> return True)
        cleanProjects
        return res

    Q.assert exceptionRaised

-- New type for a unique list of projects to add in the DB
newtype ProjectUniqueList = ProjectUniqueList [Project]
    deriving Show

-- Its arbitrary instance, make sure there is no duplicate
instance Arbitrary ProjectUniqueList where
    arbitrary = ProjectUniqueList <$> listOf arbitrary `suchThat` hasNoDups
      where hasNoDups x = length x == length (Set.fromList x) 

prop_projList :: RunDB -> ProjectUniqueList -> Property
prop_projList runDB (ProjectUniqueList projects) = Q.monadic (ioProperty . runDB) $ do
    dbProjects <- Q.run $ do
        mapM_ projAdd projects
        res <- projList
        cleanProjects
        return res

    Q.assert $ dbProjects == sort projects

testProjAPI :: RunDB -> Spec
testProjAPI runDB =
    describe "Test the project API" $ do
        context "When the DB is empty" $ do
            it "tests the uniqueness of the name" $
                runDB (projAdd project1 >> projAdd project1)
                  `shouldThrow` modelException
            it "tests if a project does not exists" $
                runDB (projExists project1) `shouldReturn` False
            it "tests if we can remove a project" $
                runDB (projRm project1) `shouldThrow` modelException
            it "tests if we can rename a project not present in the db" $
                runDB (projRename project1 project2)
                    `shouldThrow` modelException
            it "tests the list of projects" $
                runDB projList `shouldReturn` []
        context "One project in DB"
            $ before_ (runDB (projAdd project1))
            $ after_ (runDB cleanProjects)
            $ do
            it "tests if the project exists" $
                runDB (projExists project1) `shouldReturn` True
            it "tests if we can remove the project" $  do
                exists <- runDB $ do
                    projRm project1
                    projExists project1
                exists `shouldBe` False
            it "tests if we can rename it" $ do
                runDB $ projRename project1 project2
                proj1Exists <- runDB (projExists project1)
                proj1Exists `shouldBe` False
                proj2Exists <- runDB (projExists project2)
                proj2Exists `shouldBe` True
            it "tests the list of projects" $
                runDB projList `shouldReturn` [project1]
        context "Test properties" $ do
            it "projAdd projExists" $
                property (prop_projAddProjExists runDB)
            it "projAdd projAdd" $
                property (prop_projAddProjAdd runDB)
            it "projList" $
                property (prop_projList runDB)
  where project1 = mkProjectLit $$(refineTH  "TestProject1") 
        project2 = mkProjectLit $$(refineTH "TestProject2") 

main :: IO ()
main = runNoLoggingT . withSqliteConn ":memory:" $ \conn -> liftIO $ do
    -- Setup a run function which captures the connection
    let runDB :: RunDB
        runDB = \actions -> runSqlPersistM actions conn
    -- Launch the tests
    hspec $ beforeAll (runDB $ runMigration migrateAll) $ 
        testProjAPI runDB

