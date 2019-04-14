{-# LANGUAGE TemplateHaskell #-}

import           RIO
import           RIO.List as L (sort)
import qualified RIO.Set as Set (fromList)
import qualified RIO.Time as Time (TimeOfDay(..), Day, fromGregorian)

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
    , shouldSatisfy
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

import           HalfDayType (HalfDayType(..))
import           Model 
    ( HalfDay(..)
    , HalfDayWorked(..)
    , Project(..)
    , mkProjectLit
    , migrateAll
    )
import           ModelFcts
    ( ModelException(..)
    , hdHdwProjGet
    , hdRm
    , hdSetHoliday
    , hdSetWork
    , hdwSetArrived
    , hdwSetArrivedAndLeft
    , hdwSetLeft
    , hdwSetNotes
    , hdwSetOffice
    , hdwSetProject
    , projAdd
    , projExists
    , projList
    , projRename
    , projRm
    )
import          Office (Office(..))
import          TimeInDay (TimeInDay(..))

-- | The type for the runDB function
type RunDB = (forall a. SqlPersistM a -> IO a)

-- | New type for a unique list of projects to add in the DB
newtype ProjectUniqueList = ProjectUniqueList [Project]
    deriving Show

-- | Its arbitrary instance, make sure there is no duplicate
instance Arbitrary ProjectUniqueList where
    arbitrary = ProjectUniqueList <$> listOf arbitrary `suchThat` hasNoDups
      where hasNoDups x = length x == length (Set.fromList x) 

-- | Clean up the db
cleanDB :: (MonadIO m) => SqlPersistT m ()
cleanDB = do
    deleteWhere ([] :: [Filter HalfDayWorked])
    deleteWhere ([] :: [Filter HalfDay])
    deleteWhere ([] :: [Filter Project])

-- | QuickCheck selector for our ModelException
modelException :: Selector ModelException
modelException = const True

-- Some constants for the specs

project1, project2 :: Project
project1 = mkProjectLit $$(refineTH "TestProject1") 
project2 = mkProjectLit $$(refineTH "TestProject2") 

day :: Time.Day
day = Time.fromGregorian 1979 03 22

tid :: TimeInDay
tid = Morning

arrived :: Time.TimeOfDay
arrived = Time.TimeOfDay 9 0 0

left :: Time.TimeOfDay
left = Time.TimeOfDay 12 0 0

notes :: Text
notes = "some notes"

office :: Office
office = Home

-- Properties

prop_projAddProjExists :: RunDB -> Project -> Property
prop_projAddProjExists runDB project = Q.monadic (ioProperty . runDB) $ do
    (exists, noExists) <- Q.run $ do
        projAdd project 
        exists <- projExists project
        projRm project 
        noExists <- projExists project
        cleanDB
        return (exists, noExists)

    Q.assert (exists && not noExists)

prop_projAddProjAdd :: RunDB -> Project -> Property
prop_projAddProjAdd runDB project =  Q.monadic (ioProperty . runDB) $ do
    exceptionRaised <- Q.run $ do
        projAdd project
        res <- catch (projAdd project >> return False) (\(ModelException _) -> return True)
        cleanDB
        return res

    Q.assert exceptionRaised

prop_projList :: RunDB -> ProjectUniqueList -> Property
prop_projList runDB (ProjectUniqueList projects) = Q.monadic (ioProperty . runDB) $ do
    dbProjects <- Q.run $ do
        mapM_ projAdd projects
        res <- projList
        cleanDB
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
        context "One project in DB" $ 
            before_ (runDB (projAdd project1)) $ 
            after_ (runDB cleanDB) $ do
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

testHdAPI :: RunDB -> Spec
testHdAPI runDB =
    describe "Test hd API" $ 
        after_ (runDB cleanDB) $ do
        context "When the DB is empty" $ do
            it "tests adding a holiday entry" $
                runDB (hdSetHoliday day tid) 
            it "tests adding a work entry" $ 
                runDB $  projAdd project1 
                      >> hdSetWork day tid project1
            it "tests getting an entry" $
                runDB (hdHdwProjGet day tid) `shouldThrow` modelException
            it "tests removing an entry" $
                runDB (hdRm day tid) `shouldThrow` modelException
            it "tests setting arrived time" $
                runDB (hdwSetArrived day tid arrived) `shouldThrow` modelException
            it "tests setting left time" $
                runDB (hdwSetLeft day tid left) `shouldThrow` modelException
            it "tests setting arrived and left time" $
                runDB (hdwSetArrivedAndLeft day tid arrived left) `shouldThrow` modelException
            it "tests setting notes" $
                runDB (hdwSetNotes day tid notes) `shouldThrow` modelException
            it "tests setting office" $
                runDB (hdwSetOffice day tid office) `shouldThrow` modelException
            it "tests setting the project" $
                runDB (hdwSetProject day tid project1) `shouldThrow` modelException
        context "When there is one holiday entry" $ 
            before_ (runDB $ hdSetHoliday day tid) $ do
            it "tests getting the entry" $ do
                res <- runDB (hdHdwProjGet day tid)
                res `shouldBe` (HalfDay day tid Holiday, Nothing)
            it "tests removing the entry" $
                runDB (hdRm day tid >> hdHdwProjGet day tid)
                    `shouldThrow` modelException
            it "tests setting arrived time" $
                runDB (hdwSetArrived day tid arrived) `shouldThrow` modelException
            it "tests setting left time" $
                runDB (hdwSetLeft day tid left) `shouldThrow` modelException
            it "tests setting arrived and left time" $
                runDB (hdwSetArrivedAndLeft day tid arrived left) `shouldThrow` modelException
            it "tests setting notes" $
                runDB (hdwSetNotes day tid notes) `shouldThrow` modelException
            it "tests setting office" $
                runDB (hdwSetOffice day tid office) `shouldThrow` modelException
            it "tests setting the project" $
                runDB (hdwSetProject day tid project1) `shouldThrow` modelException
        context "When there is one work entry" $ 
            before_ (runDB $ projAdd project1 >> hdSetWork day tid project1) $ do
            it "tests getting the entry" $ do
                (hd, mbHdwProj) <- runDB (hdHdwProjGet day tid)
                hd `shouldBe` HalfDay day tid Worked
                mbHdwProj `shouldSatisfy` (\case
                    Nothing -> False
                    Just (HalfDayWorked {}, proj) -> proj == project1)
            it "tests removing the entry" $
                runDB (hdRm day tid >> hdHdwProjGet day tid)
                    `shouldThrow` modelException
            it "tests setting arrived time" $ do
                runDB (hdwSetArrived day tid arrived) 
                (_, mbHdwProj) <- runDB (hdHdwProjGet day tid)
                mbHdwProj `shouldSatisfy` (\case
                    Nothing -> False
                    Just (HalfDayWorked {halfDayWorkedArrived}, _) -> 
                        halfDayWorkedArrived == arrived)
            it "tests setting left time" $ do
                runDB (hdwSetLeft day tid left) 
                (_, mbHdwProj) <- runDB (hdHdwProjGet day tid)
                mbHdwProj `shouldSatisfy` (\case
                    Nothing -> False
                    Just (HalfDayWorked {halfDayWorkedLeft}, _) -> 
                        halfDayWorkedLeft == left)
            it "tests setting arrived and left time" $ do
                runDB (hdwSetArrivedAndLeft day tid arrived left) 
                (_, mbHdwProj) <- runDB (hdHdwProjGet day tid)
                mbHdwProj `shouldSatisfy` (\case
                    Nothing -> False
                    Just (HalfDayWorked {halfDayWorkedArrived, halfDayWorkedLeft}, _) -> 
                        halfDayWorkedArrived == arrived && halfDayWorkedLeft == left)
            it "tests setting notes" $ do
                runDB (hdwSetNotes day tid notes)
                (_, mbHdwProj) <- runDB (hdHdwProjGet day tid)
                mbHdwProj `shouldSatisfy` (\case
                    Nothing -> False
                    Just (HalfDayWorked {halfDayWorkedNotes}, _) -> 
                        halfDayWorkedNotes == notes)
            it "tests setting office" $ do
                runDB (hdwSetOffice day tid office)
                (_, mbHdwProj) <- runDB (hdHdwProjGet day tid)
                mbHdwProj `shouldSatisfy` (\case
                    Nothing -> False
                    Just (HalfDayWorked {halfDayWorkedOffice}, _) -> 
                        halfDayWorkedOffice == office)
            it "tests setting the project" $ do
                runDB (projAdd project2 >> hdwSetProject day tid project2)
                (_, mbHdwProj) <- runDB (hdHdwProjGet day tid)
                mbHdwProj `shouldSatisfy` (\case
                    Nothing -> False
                    Just (_, project) -> project == project2)

main :: IO ()
main = runNoLoggingT . withSqliteConn "test.db" $ \conn -> liftIO $ do
    -- Setup a run function which captures the connection
    let runDB :: RunDB
        runDB actions = runSqlPersistM actions conn
    -- Launch the tests
    hspec $ beforeAll (runDB $ runMigration migrateAll) $ do
        testProjAPI runDB
        testHdAPI runDB

