{-# LANGUAGE TemplateHaskell #-}

import           RIO
import           RIO.List as L (sort)
import qualified RIO.Set as Set (fromList)
import qualified RIO.Text as Text (Text)
import qualified RIO.Time as Time (TimeOfDay(..), Day, fromGregorian)

import           Control.Monad (ap)
import           Control.Monad.Logger (runNoLoggingT)

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
import           Test.QuickCheck.Instances.Time()

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

day1 :: Time.Day
day1 = Time.fromGregorian 1979 03 22

tid1 :: TimeInDay
tid1 = Morning

arrived1 :: Time.TimeOfDay
arrived1 = Time.TimeOfDay 9 0 0

left1 :: Time.TimeOfDay
left1 = Time.TimeOfDay 12 0 0

notes1 :: Text
notes1 = "some notes"

office1 :: Office
office1 = Home

-- Properties

-- | Test adding a project in the DB and testing if it exists
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

-- | Make sure it raises an exception when we add twice the same project
prop_projAddProjAdd :: RunDB -> Project -> Property
prop_projAddProjAdd runDB project =  Q.monadic (ioProperty . runDB) $ do
    exceptionRaised <- Q.run $ do
        projAdd project
        res <- catch (projAdd project >> return False) (\(ModelException _) -> return True)
        cleanDB
        return res

    Q.assert exceptionRaised

-- | Test if projList return the actual list of the projects added to the DB
prop_projList :: RunDB -> ProjectUniqueList -> Property
prop_projList runDB (ProjectUniqueList projects) = Q.monadic (ioProperty . runDB) $ do
    dbProjects <- Q.run $ do
        mapM_ projAdd projects
        res <- projList
        cleanDB
        return res

    Q.assert $ dbProjects == sort projects

-- | Test the presence of a holiday entry
prop_hdSetHoliday :: RunDB -> Time.Day -> TimeInDay -> Property
prop_hdSetHoliday runDB day tid = Q.monadic (ioProperty . runDB) $ do
    (hd, mbHdwProj) <- Q.run $ do
        hdSetHoliday day tid
        res <- hdHdwProjGet day tid
        cleanDB
        return res

    Q.assert $ halfDayType hd == Holiday && mbHdwProj == Nothing

-- | Test the presence of a worked entry
prop_hdSetWork :: RunDB -> Time.Day -> TimeInDay -> Project -> Property
prop_hdSetWork runDB day tid project = Q.monadic (ioProperty . runDB) $ do
    -- Impossible to set a work hd without an existing project
    exceptionRaised <- Q.run $ do
        res <- catch (hdSetWork day tid project >> return False) (\(ModelException _) -> return True)
        cleanDB
        return res

    Q.assert exceptionRaised

    -- With a project no problem
    (hd, mbHdwProj) <- Q.run $ do
        projAdd project
        hdSetWork day tid project
        res <- hdHdwProjGet day tid
        cleanDB
        return res

    Q.assert $ halfDayType hd == Worked && checkProject mbHdwProj

  where checkProject (Just (_, project')) = project' == project
        checkProject _ = False

-- | Test the set arrived function
prop_hdSetArrived :: RunDB -> Time.Day -> TimeInDay -> Project -> Time.TimeOfDay -> Property
prop_hdSetArrived runDB day tid project tod = Q.monadic (ioProperty . runDB) $ do
    -- Initialize the hdw
    (_, mbHdwProj) <- Q.run $ do
        projAdd project
        hdSetWork day tid project
        hdHdwProjGet day tid
    
    -- Update arrived time and get new value
    exceptionRaised <- Q.run $ catch (hdwSetArrived day tid tod >> return False) (\(ModelException _) -> return True)
    (_, mbHdwProj') <- Q.run $ hdHdwProjGet day tid
    Q.run $ cleanDB

    let inRange = maybe False (\((HalfDayWorked _ _ left _ _ _), _) -> tod < left) mbHdwProj
        inDB = maybe False (\((HalfDayWorked _ arrived _ _ _ _), _) -> tod == arrived) mbHdwProj'

    Q.assert $ inRange /= exceptionRaised && inRange == inDB

-- | Test the set left function
prop_hdSetLeft :: RunDB -> Time.Day -> TimeInDay -> Project -> Time.TimeOfDay -> Property
prop_hdSetLeft runDB day tid project tod = Q.monadic (ioProperty . runDB) $ do
    -- Initialize the hdw
    (_, mbHdwProj) <- Q.run $ do
        projAdd project
        hdSetWork day tid project
        hdHdwProjGet day tid
    
    -- Update left time and get new value
    exceptionRaised <- Q.run $ catch (hdwSetLeft day tid tod >> return False) (\(ModelException _) -> return True)
    (_, mbHdwProj') <- Q.run $ hdHdwProjGet day tid
    Q.run $ cleanDB

    let inRange = maybe False (\((HalfDayWorked _ arrived _ _ _ _), _) -> tod > arrived) mbHdwProj
        inDB = maybe False (\((HalfDayWorked _ _ left _ _ _), _) -> tod == left) mbHdwProj'

    Q.assert $ inRange /= exceptionRaised && inRange == inDB

-- | Test setting arrived and left time
prop_hdSetArrivedAndLeft 
    :: RunDB 
    -> Time.Day 
    -> TimeInDay 
    -> Project 
    -> Time.TimeOfDay 
    -> Time.TimeOfDay 
    -> Property
prop_hdSetArrivedAndLeft runDB day tid project arrived left = Q.monadic (ioProperty . runDB) $ do
    -- Initialize the hdw
    _ <- Q.run $ do
        projAdd project
        hdSetWork day tid project
        hdHdwProjGet day tid
    
    -- Update times and get new value
    exceptionRaised <- Q.run $ catch (hdwSetArrivedAndLeft day tid arrived left >> return False) (\(ModelException _) -> return True)
    (_, mbHdwProj') <- Q.run $ hdHdwProjGet day tid
    Q.run $ cleanDB

    let inRange = arrived < left
        inDB = maybe False (\((HalfDayWorked _ arrived' left' _ _ _), _) -> arrived' == arrived && left == left') mbHdwProj'

    Q.assert $ inRange /= exceptionRaised && inRange == inDB

-- | Test setting the notes
prop_hdSetNotes :: RunDB -> Time.Day -> TimeInDay -> Project -> Text.Text-> Property
prop_hdSetNotes runDB day tid project notes = Q.monadic (ioProperty . runDB) $ do
    -- Initialize the hdw and set the notes
    (_, mbHdwProj) <- Q.run $ do
        projAdd project
        hdSetWork day tid project
        hdwSetNotes day tid notes
        res <- hdHdwProjGet day tid
        cleanDB
        return res

    Q.assert $ maybe False (\((HalfDayWorked notes' _ _ _ _ _), _) -> notes' == notes) mbHdwProj

-- | Test setting the office
prop_hdSetOffice :: RunDB -> Time.Day -> TimeInDay -> Project -> Office-> Property
prop_hdSetOffice runDB day tid project office = Q.monadic (ioProperty . runDB) $ do
    -- Initialize the hdw and set the office
    (_, mbHdwProj) <- Q.run $ do
        projAdd project
        hdSetWork day tid project
        hdwSetOffice day tid office
        res <- hdHdwProjGet day tid
        cleanDB
        return res

    Q.assert $ maybe False (\((HalfDayWorked _ _ _ office' _ _), _) -> office' == office) mbHdwProj

-- | Test the project API
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

-- | When there is no work entry, all these should throw an exception
itemsNoWorkedEntry :: RunDB -> Spec
itemsNoWorkedEntry runDB = do
    it "tests setting arrived time" $
        runDB (hdwSetArrived day1 tid1 arrived1) `shouldThrow` modelException
    it "tests setting left time" $
        runDB (hdwSetLeft day1 tid1 left1) `shouldThrow` modelException
    it "tests setting arrived and left time" $
        runDB (hdwSetArrivedAndLeft day1 tid1 arrived1 left1) `shouldThrow` modelException
    it "tests setting notes" $
        runDB (hdwSetNotes day1 tid1 notes1) `shouldThrow` modelException
    it "tests setting office" $
        runDB (hdwSetOffice day1 tid1 office1) `shouldThrow` modelException
    it "tests setting the project" $
        runDB (hdwSetProject day1 tid1 project1) `shouldThrow` modelException

-- | Test the HD API
testHdAPI :: RunDB -> Spec
testHdAPI runDB =
    describe "Test hd API" $ 
        after_ (runDB cleanDB) $ do
        context "When the DB is empty" $ do
            it "tests adding a holiday entry" $
                runDB (hdSetHoliday day1 tid1) 
            it "tests adding a work entry" $ 
                runDB $  projAdd project1 
                      >> hdSetWork day1 tid1 project1
            it "tests getting an entry" $
                runDB (hdHdwProjGet day1 tid1) `shouldThrow` modelException
            it "tests removing an entry" $
                runDB (hdRm day1 tid1) `shouldThrow` modelException
            itemsNoWorkedEntry runDB
        context "When there is one holiday entry" $ 
            before_ (runDB $ hdSetHoliday day1 tid1) $ do
            it "tests getting the entry" $ do
                res <- runDB (hdHdwProjGet day1 tid1)
                res `shouldBe` (HalfDay day1 tid1 Holiday, Nothing)
            it "tests removing the entry" $ do
                runDB (hdRm day1 tid1) 
                runDB (hdHdwProjGet day1 tid1) `shouldThrow` modelException
            it "tests overriding with a worked entry" $ 
                runDB $ projAdd project1 >> hdSetWork day1 tid1 project1
            itemsNoWorkedEntry runDB
        context "When there is one work entry" $ 
            before_ (runDB $ projAdd project1 >> hdSetWork day1 tid1 project1) $ do
            it "tests getting the entry" $ do
                (hd, mbHdwProj) <- runDB (hdHdwProjGet day1 tid1)
                hd `shouldBe` HalfDay day1 tid1 Worked
                mbHdwProj `projShouldBe` project1
            it "tests removing the entry" $ do
                runDB (hdRm day1 tid1)
                runDB (hdHdwProjGet day1 tid1) `shouldThrow` modelException
            it "tests overriding with a holiday entry" $ 
                runDB $ hdSetHoliday day1 tid1
            it "tests setting arrived time" $ do
                runDB (hdwSetArrived day1 tid1 arrived1) 
                (_, mbHdwProj) <- runDB (hdHdwProjGet day1 tid1)
                mbHdwProj `hdwShouldSatisfy` ((==) arrived1 . halfDayWorkedArrived)
            it "tests setting left time" $ do
                runDB (hdwSetLeft day1 tid1 left1) 
                (_, mbHdwProj) <- runDB (hdHdwProjGet day1 tid1)
                mbHdwProj `hdwShouldSatisfy` ((==) left1 . halfDayWorkedLeft)
            it "tests setting arrived and left time" $ do
                runDB (hdwSetArrivedAndLeft day1 tid1 arrived1 left1) 
                (_, mbHdwProj) <- runDB (hdHdwProjGet day1 tid1)
                -- We check that arrived and left times are good
                mbHdwProj `hdwShouldSatisfy` (and . ap [ (==) arrived1 . halfDayWorkedArrived, 
                                                         (==) left1 . halfDayWorkedLeft ] . pure) 
            it "tests setting notes" $ do
                runDB (hdwSetNotes day1 tid1 notes1)
                (_, mbHdwProj) <- runDB (hdHdwProjGet day1 tid1)
                mbHdwProj `hdwShouldSatisfy` ((==) notes1 . halfDayWorkedNotes)
            it "tests setting office" $ do
                runDB (hdwSetOffice day1 tid1 office1)
                (_, mbHdwProj) <- runDB (hdHdwProjGet day1 tid1)
                mbHdwProj `hdwShouldSatisfy` ((==) office1 . halfDayWorkedOffice)
            it "tests setting the project" $ do
                runDB (projAdd project2 >> hdwSetProject day1 tid1 project2)
                (_, mbHdwProj) <- runDB (hdHdwProjGet day1 tid1)
                mbHdwProj `projShouldBe` project2
        context "Test properties" $ do
            it "prop_hdSetHoliday" $
                property (prop_hdSetHoliday runDB)
            it "prop_hdSetWork" $
                property (prop_hdSetWork runDB)
            it "prop_hdSetArrived" $
                property (prop_hdSetArrived runDB)
            it "prop_hdSetLeft" $
                property (prop_hdSetLeft runDB)
            it "prop_hdSetArrivedAndLeft" $ 
                property (prop_hdSetArrivedAndLeft runDB)
            -- it "prop_hdSetNotes" $ 
            --     property (prop_hdSetNotes runDB)
            it "prop_hdSetOffice" $ 
                property (prop_hdSetOffice runDB)
  where 
    projShouldBe mbHdwProj proj = mbHdwProj `shouldSatisfy` maybe False ((==) proj . snd)
    hdwShouldSatisfy mbHdwProj pred = mbHdwProj `shouldSatisfy` maybe False (pred . fst)

-- | Main function
main :: IO ()
main = runNoLoggingT . withSqliteConn ":memory:" $ \conn -> liftIO $ do
    -- Setup a run function which captures the connection
    let runDB :: RunDB
        runDB actions = runSqlPersistM actions conn
    -- Launch the tests
    hspec $ beforeAll (runDB $ runMigration migrateAll) $ do
        testProjAPI runDB
        testHdAPI runDB

