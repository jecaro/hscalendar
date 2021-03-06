{-# LANGUAGE TemplateHaskell #-}

-- | The test suite
module Main where

import Control.Monad (ap)
import Control.Monad.Logger (runNoLoggingT)
import Database.Persist.Sql
    ( SqlPersistM,
      SqlPersistT,
      runMigration,
      runSqlPersistM,
      runSqlPersistM,
    )
import Database.Persist.Sqlite (withSqliteConn)
import Db.HalfDay (HalfDay (..))
import Db.Login (Login, mkLoginLit)
import Db.Model
    ( HdNotFound (..),
      ProjExists (..),
      ProjHasHd (..),
      ProjNotFound (..),
      TimesAreWrong (..),
      UserExists (..),
      UserNotFound (..),
      cleanDB,
      hdGet,
      hdRm,
      hdSetArrived,
      hdSetArrivedAndLeft,
      hdSetLeft,
      hdSetNotes,
      hdSetOff,
      hdSetOffice,
      hdSetProject,
      hdSetWork,
      migrateAll,
      monthGet,
      projAdd,
      projExists,
      projList,
      projRename,
      projRm,
      userAdd,
      userChangePassword,
      userCheck,
      userExists,
      userList,
      userRename,
      userRm,
      weekGet,
    )
import qualified Db.Month as Month (Month, fromDay)
import qualified Db.MonthF as MonthF (MonthWithDays, add, empty)
import Db.Notes (Notes, mkNotesLit)
import Db.Off (Off (..))
import qualified Db.OffDayType as ODT (OffDayType (..))
import Db.Office (Office (..))
import Db.Password (Password, mkPasswordLit)
import Db.Project (Project, mkProjectLit)
import Db.TimeInDay (TimeInDay (..), other)
import qualified Db.Week as Week (Week, fromDay)
import qualified Db.WeekF as WeekF (WeekWithDays, add, empty)
import Db.Worked (Worked (..))
import RIO
import qualified RIO.List as L (sort)
import qualified RIO.Set as Set (fromList)
import qualified RIO.Time as Time (Day, TimeOfDay (..), fromGregorian)
import Refined (refineTH)
import Test.Hspec
    ( Selector,
      Spec,
      after_,
      beforeAll,
      before_,
      context,
      describe,
      expectationFailure,
      hspec,
      it,
      shouldBe,
      shouldReturn,
      shouldSatisfy,
      shouldThrow,
    )
import Test.QuickCheck
    ( Arbitrary (..),
      Property,
      ioProperty,
      listOf,
      property,
      suchThat,
    )
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()
import qualified Test.QuickCheck.Monadic as Q (PropertyM, assert, monadic, run)

-- | The type for the runDB function
type RunDB = (forall a. SqlPersistM a -> IO a)

-- | New type for a unique list of projects to add in the DB
newtype ProjectUniqueList = ProjectUniqueList [Project]
    deriving (Show)

-- | Its arbitrary instance, make sure there is no duplicate
instance Arbitrary ProjectUniqueList where
    arbitrary = ProjectUniqueList <$> listOf arbitrary `suchThat` hasNoDups
        where
            hasNoDups x = length x == length (Set.fromList x)

-- | New type for a unique list of users to add in the DB: a tuple with a login
-- first then the password
newtype UserUniqueList = UserUniqueList [(Login, Password)]
    deriving (Show)

-- | Its arbitrary instance, make sure there is no duplicate login
instance Arbitrary UserUniqueList where
    arbitrary = UserUniqueList <$> listOf arbitrary `suchThat` hasNoDups
        where
            hasNoDups x = length x == length (Set.fromList (fst <$> x))

-- | hspec selector for 'ProjExists' exception
projExistsException :: Selector ProjExists
projExistsException = const True

-- | hspec selector for 'ProjNotFound' exception
projNotFoundException :: Selector ProjNotFound
projNotFoundException = const True

-- | hspec selector for 'ProjHasHd' exception
projHasHDWException :: Selector ProjHasHd
projHasHDWException = const True

-- | hspec selector for 'HdNotFound' exception
hdNotFoundException :: Selector HdNotFound
hdNotFoundException = const True

-- | hspec selector for 'UserExists' exception
userExistsException :: Selector UserExists
userExistsException = const True

-- | hspec selector for 'UserNotFound exception
userNotFoundException :: Selector UserNotFound
userNotFoundException = const True

-- Some constants for the specs

project1, project2 :: Project
project1 = mkProjectLit $$(refineTH "TestProject1")
project2 = mkProjectLit $$(refineTH "TestProject2")

day1 :: Time.Day
day1 = Time.fromGregorian 1979 03 22

week1 :: Week.Week
week1 = fst $ Week.fromDay day1

emptyWeek1 :: WeekF.WeekWithDays
emptyWeek1 = WeekF.empty Nothing week1

month1 :: Month.Month
month1 = Month.fromDay day1

emptyMonth1 :: MonthF.MonthWithDays
emptyMonth1 = MonthF.empty Nothing month1

tid1 :: TimeInDay
tid1 = Morning

notes1 :: Notes
notes1 = mkNotesLit $$(refineTH "some notes")

office1 :: Office
office1 = Home

hdt1 :: ODT.OffDayType
hdt1 = ODT.PaidLeave

hdt1' :: ODT.OffDayType
hdt1' = ODT.PaidLeave

arrived1 :: TimeInDay -> Time.TimeOfDay
arrived1 Morning = Time.TimeOfDay 8 30 0
arrived1 Afternoon = Time.TimeOfDay 13 30 0

left1 :: TimeInDay -> Time.TimeOfDay
left1 Morning = Time.TimeOfDay 12 0 0
left1 Afternoon = Time.TimeOfDay 17 0 0

user1 :: Login
user1 = mkLoginLit $$(refineTH "login1")

user2 :: Login
user2 = mkLoginLit $$(refineTH "login2")

password1 :: Password
password1 = mkPasswordLit $$(refineTH "We@kP@ssw0rd")

password2 :: Password
password2 = mkPasswordLit $$(refineTH "@n0therWe@kP@ssw0rd")

defaultCost :: Int
defaultCost = 4

defaultWorked :: Time.Day -> TimeInDay -> Project -> HalfDay
defaultWorked day tid project =
    MkHalfDayWorked
        ( MkWorked
              day
              tid
              (arrived1 tid)
              (left1 tid)
              office1
              defaultNotes
              project
        )
    where
        defaultNotes = mkNotesLit $$(refineTH "")

-- Helper functions to simplify the tests

toMbWorked :: HalfDay -> Maybe Worked
toMbWorked (MkHalfDayOff _) = Nothing
toMbWorked (MkHalfDayWorked worked) = Just worked

-- | Unwrap Project and test equality
checkProj :: Project -> Maybe Worked -> Bool
checkProj proj = maybe False ((==) proj . _workedProject)

-- | hdSetWork with default value for arrived/left
hdSetWorkDefault :: MonadUnliftIO m => Time.Day -> TimeInDay -> Project -> SqlPersistT m ()
hdSetWorkDefault day tid project = hdSetWork day tid project office1 (arrived1 tid) (left1 tid)

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
        pure (exists, noExists)

    Q.assert (exists && not noExists)

-- | Make sure it raises an exception when we add twice the same project
prop_projAddProjAdd :: RunDB -> Project -> Property
prop_projAddProjAdd runDB project = Q.monadic (ioProperty . runDB) $ do
    exceptionRaised <- Q.run $ do
        projAdd project
        res <- catch (projAdd project >> pure False) (\(ProjExists _) -> pure True)
        cleanDB
        pure res

    Q.assert exceptionRaised

-- | Test if projList return the actual list of the projects added to the DB
prop_projList :: RunDB -> ProjectUniqueList -> Property
prop_projList runDB (ProjectUniqueList projects) = Q.monadic (ioProperty . runDB) $ do
    dbProjects <- Q.run $ do
        mapM_ projAdd projects
        res <- projList
        cleanDB
        pure res

    Q.assert $ dbProjects == L.sort projects

-- | Test the presence of a day off
prop_hdSetOff :: RunDB -> Time.Day -> TimeInDay -> ODT.OffDayType -> Property
prop_hdSetOff runDB day tid hdt = Q.monadic (ioProperty . runDB) $ do
    Q.run $ hdSetOff day tid hdt

    -- Check if the value in the database is right
    hd <- Q.run $ hdGet day tid

    case hd of
        MkHalfDayWorked _ -> Q.assert False
        MkHalfDayOff (MkOff _ _ hdt') -> Q.assert $ hdt == hdt'

    Q.run cleanDB

-- | Test that hdSetWork raises an exception if the project does not exists
prop_hdSetWorkNoProj :: RunDB -> Time.Day -> TimeInDay -> Project -> Property
prop_hdSetWorkNoProj runDB day tid project = Q.monadic (ioProperty . runDB) $ do
    -- Impossible to set a work hd without an existing project
    exceptionRaised <- Q.run $ do
        res <- catch (hdSetWorkDefault day tid project >> pure False) (\(ProjNotFound _) -> pure True)
        cleanDB
        pure res

    Q.assert exceptionRaised

-- | Test that hdSetWork raises an exception if the times are wrong
prop_hdSetWork ::
    RunDB ->
    Time.Day ->
    TimeInDay ->
    Project ->
    Office ->
    Time.TimeOfDay ->
    Time.TimeOfDay ->
    Property
prop_hdSetWork runDB day tid project office arrived left = Q.monadic (ioProperty . runDB) $ do
    exceptionRaised <-
        Q.run $
            catch
                ( do
                      projAdd project
                      hdSetWorkDefault day (other tid) project
                      hdSetWork day tid project office arrived left >> pure False
                )
                (\TimesAreWrong -> pure True)

    testDBAndTimes day tid arrived left exceptionRaised
    Q.run cleanDB

-- | Check if:
--   - the times are ok and are in DB
--   - the times are not ok and an exception was raised
testDBAndTimes ::
    MonadUnliftIO m =>
    Time.Day ->
    TimeInDay ->
    Time.TimeOfDay ->
    Time.TimeOfDay ->
    Bool ->
    Q.PropertyM (SqlPersistT m) ()
testDBAndTimes day tid arrived left exceptionRaised = do
    (mbWorked, mbOWorked) <- Q.run $ do
        -- current is not necessary present in the DB, especially if there is
        -- something wrong with the times
        eiHd <- try (hdGet day tid)
        let mbWorked = case eiHd of
                Left (HdNotFound _ _) -> Nothing
                Right entity -> toMbWorked entity
        -- other is always present in the DB
        mbOWorked <- toMbWorked <$> hdGet day (other tid)
        pure (mbWorked, mbOWorked)

    let beforeOtherArrived = beforeArrived left mbOWorked
        afterOtherLeft = afterLeft arrived mbOWorked
        inRange = arrived < left && (tid == Morning && beforeOtherArrived || tid == Afternoon && afterOtherLeft)
        inDB = arrivedEquals arrived mbWorked && leftEquals left mbWorked

    Q.assert $ inRange /= exceptionRaised && inRange == inDB

-- Some helper functions below for the time related properties

beforeLeft :: Time.TimeOfDay -> Maybe Worked -> Bool
beforeLeft tod = maybe False ((<) tod . _workedLeft)

afterLeft :: Time.TimeOfDay -> Maybe Worked -> Bool
afterLeft tod = maybe False ((>=) tod . _workedLeft)

beforeArrived :: Time.TimeOfDay -> Maybe Worked -> Bool
beforeArrived tod = maybe False ((<=) tod . _workedArrived)

afterArrived :: Time.TimeOfDay -> Maybe Worked -> Bool
afterArrived tod = maybe False ((>) tod . _workedArrived)

arrivedEquals :: Time.TimeOfDay -> Maybe Worked -> Bool
arrivedEquals tod = maybe False ((==) tod . _workedArrived)

leftEquals :: Time.TimeOfDay -> Maybe Worked -> Bool
leftEquals tod = maybe False ((==) tod . _workedLeft)

-- | Test the set arrived function
prop_hdSetArrived :: RunDB -> Time.Day -> TimeInDay -> Project -> Time.TimeOfDay -> Property
prop_hdSetArrived runDB day tid project tod = Q.monadic (ioProperty . runDB) $ do
    -- Initialize the two hdws
    (mbWorked, mbOWorked) <- Q.run $ do
        projAdd project
        hdSetWorkDefault day Morning project
        hdSetWorkDefault day Afternoon project
        mbWorked <- toMbWorked <$> hdGet day tid
        mbOWorked <- toMbWorked <$> hdGet day (other tid)
        -- Return current and other hdw
        pure (mbWorked, mbOWorked)

    -- Update arrived time and get new value
    exceptionRaised <-
        Q.run $
            catch
                (hdSetArrived day tid tod >> pure False)
                (\TimesAreWrong -> pure True)
    mbWorked' <- toMbWorked <$> Q.run (hdGet day tid)
    Q.run cleanDB

    -- In all case, we need arrive before left
    let beforeCurLeft = beforeLeft tod mbWorked
        -- And after morning left if need be
        afterOtherLeft = afterLeft tod mbOWorked
        inRange = beforeCurLeft && (tid == Morning || afterOtherLeft)
        inDB = arrivedEquals tod mbWorked'

    Q.assert $ inRange /= exceptionRaised && inRange == inDB

-- | Test the set left function
prop_hdSetLeft :: RunDB -> Time.Day -> TimeInDay -> Project -> Time.TimeOfDay -> Property
prop_hdSetLeft runDB day tid project tod = Q.monadic (ioProperty . runDB) $ do
    -- Initialize the two hdws
    (mbWorked, mbOWorked) <- Q.run $ do
        projAdd project
        hdSetWorkDefault day Morning project
        hdSetWorkDefault day Afternoon project
        mbWorked <- toMbWorked <$> hdGet day tid
        mbOWorked <- toMbWorked <$> hdGet day (other tid)
        -- Return current and other hdw
        pure (mbWorked, mbOWorked)

    -- Update left time and get new value
    exceptionRaised <-
        Q.run $
            catch
                (hdSetLeft day tid tod >> pure False)
                (\TimesAreWrong -> pure True)
    mbWorked' <- toMbWorked <$> Q.run (hdGet day tid)
    Q.run cleanDB

    -- We all case, we need to leave after arrived
    let afterCurArrived = afterArrived tod mbWorked
        -- And before afternoon arrived if need be
        beforeOtherArrived = beforeArrived tod mbOWorked
        inRange = afterCurArrived && (tid == Afternoon || beforeOtherArrived)
        inDB = leftEquals tod mbWorked'

    Q.assert $ inRange /= exceptionRaised && inRange == inDB

-- | Test setting arrived and left time
prop_hdSetArrivedAndLeft ::
    RunDB ->
    Time.Day ->
    TimeInDay ->
    Project ->
    Time.TimeOfDay ->
    Time.TimeOfDay ->
    Property
prop_hdSetArrivedAndLeft runDB day tid project arrived left = Q.monadic (ioProperty . runDB) $ do
    -- Initialize the two hdws and get other hdw
    Q.run $ do
        projAdd project
        hdSetWorkDefault day Morning project
        hdSetWorkDefault day Afternoon project

    -- Update times and get new value
    exceptionRaised <-
        Q.run $
            catch
                (hdSetArrivedAndLeft day tid arrived left >> pure False)
                (\TimesAreWrong -> pure True)

    testDBAndTimes day tid arrived left exceptionRaised

    Q.run cleanDB

-- | Test setting the notes
prop_hdSetNotes :: RunDB -> Time.Day -> TimeInDay -> Project -> Notes -> Property
prop_hdSetNotes runDB day tid project notes = Q.monadic (ioProperty . runDB) $ do
    -- Initialize the hdw and set the notes
    mbWorked <- Q.run $ do
        projAdd project
        hdSetWorkDefault day tid project
        hdSetNotes day tid notes
        res <- toMbWorked <$> hdGet day tid
        cleanDB
        pure res

    Q.assert $ maybe False ((==) notes . _workedNotes) mbWorked

-- | Test setting the office
prop_hdSetOffice :: RunDB -> Time.Day -> TimeInDay -> Project -> Office -> Property
prop_hdSetOffice runDB day tid project office = Q.monadic (ioProperty . runDB) $ do
    -- Initialize the hdw and set the office
    mbWorked <- Q.run $ do
        projAdd project
        hdSetWorkDefault day tid project
        hdSetOffice day tid office
        res <- toMbWorked <$> hdGet day tid
        cleanDB
        pure res

    Q.assert $ maybe False ((==) office . _workedOffice) mbWorked

-- | Test the set project function
prop_hdSetProject :: RunDB -> Time.Day -> TimeInDay -> Project -> Project -> Property
prop_hdSetProject runDB day tid project project' = Q.monadic (ioProperty . runDB) $ do
    -- Initialize the hdw
    Q.run $ do
        projAdd project
        hdSetWorkDefault day tid project

    -- Update project and get new value
    exceptionRaised <-
        Q.run $
            catch
                (hdSetProject day tid project' >> pure False)
                (\(ProjNotFound _) -> pure True)
    mbWorked <- toMbWorked <$> Q.run (hdGet day tid)
    Q.run cleanDB

    let inDB = checkProj project' mbWorked

    Q.assert $ inDB /= exceptionRaised

-- | Test adding a user and testing if it exists
prop_userAddUserExists :: RunDB -> Login -> Password -> Property
prop_userAddUserExists runDB login password = Q.monadic (ioProperty . runDB) $ do
    (exists, checks, noExists) <- Q.run $ do
        userAdd login password defaultCost
        exists <- userExists login
        checks <- userCheck login password
        userRm login
        noExists <- userExists login
        cleanDB
        pure (exists, checks, noExists)

    Q.assert (exists && checks && not noExists)

-- | Make sure it raises an exception when we add twice the same user
prop_userAddUserAdd :: RunDB -> Login -> Password -> Property
prop_userAddUserAdd runDB login password = Q.monadic (ioProperty . runDB) $ do
    exceptionRaised <- Q.run $ do
        userAdd login password defaultCost
        res <-
            catch
                (userAdd login password defaultCost >> pure False)
                (\(UserExists _) -> pure True)
        cleanDB
        pure res

    Q.assert exceptionRaised

-- | Test if userList return the actual list of the users added to the DB
prop_userList :: RunDB -> UserUniqueList -> Property
prop_userList runDB (UserUniqueList users) = Q.monadic (ioProperty . runDB) $ do
    dbUsers <- Q.run $ do
        mapM_ (\(u, p) -> userAdd u p defaultCost) users
        res <- userList
        cleanDB
        pure res

    Q.assert $ dbUsers == L.sort (fst <$> users)

-- | Test the project API
testProjApi :: RunDB -> Spec
testProjApi runDB =
    describe "Test the project API" $ do
        context "When the DB is empty" $ do
            it "tests the uniqueness of the name" $
                runDB (projAdd project1 >> projAdd project1)
                    `shouldThrow` projExistsException
            it "tests if a project does not exists" $
                runDB (projExists project1) `shouldReturn` False
            it "tests if we can remove a project" $
                runDB (projRm project1) `shouldThrow` projNotFoundException
            it "tests if we can rename a project not present in the db" $
                runDB (projRename project1 project2)
                    `shouldThrow` projNotFoundException
            it "tests the list of projects" $
                runDB projList `shouldReturn` []
        context "One project in DB"
            $ before_ (runDB (projAdd project1))
            $ after_ (runDB cleanDB)
            $ do
                it "tests if the project exists" $
                    runDB (projExists project1) `shouldReturn` True
                it "tests if we can remove the project" $ do
                    exists <- runDB $ do
                        projRm project1
                        projExists project1
                    exists `shouldBe` False
                it "tests if we can remove the project if it is used" $
                    runDB (hdSetWorkDefault day1 tid1 project1 >> projRm project1)
                        `shouldThrow` projHasHDWException
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
        runDB (hdSetArrived day1 tid1 (arrived1 tid1)) `shouldThrow` hdNotFoundException
    it "tests setting left time" $
        runDB (hdSetLeft day1 tid1 (left1 tid1)) `shouldThrow` hdNotFoundException
    it "tests setting arrived and left time" $
        runDB (hdSetArrivedAndLeft day1 tid1 (arrived1 tid1) (left1 tid1)) `shouldThrow` hdNotFoundException
    it "tests setting notes" $
        runDB (hdSetNotes day1 tid1 notes1) `shouldThrow` hdNotFoundException
    it "tests setting office" $
        runDB (hdSetOffice day1 tid1 office1) `shouldThrow` hdNotFoundException
    it "tests setting the project" $
        runDB (hdSetProject day1 tid1 project1) `shouldThrow` hdNotFoundException

-- | Test the HD API
testHdApi :: RunDB -> Spec
testHdApi runDB =
    describe "Test hd API"
        $ after_ (runDB cleanDB)
        $ do
            context "When the DB is empty" $ do
                it "tests adding a day off" $
                    runDB (hdSetOff day1 tid1 hdt1)
                it "tests adding a work entry"
                    $ runDB
                    $ projAdd project1
                        >> hdSetWorkDefault day1 tid1 project1
                it "tests getting an entry" $
                    runDB (hdGet day1 tid1) `shouldThrow` hdNotFoundException
                it "tests getting the full week" $ do
                    res <- runDB (weekGet week1)
                    res `shouldBe` emptyWeek1
                it "tests getting the full month" $ do
                    res <- runDB (monthGet month1)
                    res `shouldBe` emptyMonth1
                it "tests removing an entry" $
                    runDB (hdRm day1 tid1) `shouldThrow` hdNotFoundException
                -- No hd in the DB
                itemsNoWorkedEntry runDB
            context "When there is a day off"
                $ before_ (runDB $ hdSetOff day1 tid1 hdt1)
                $ do
                    it "tests getting the entry" $ do
                        res <- runDB (hdGet day1 tid1)
                        res `shouldBe` MkHalfDayOff (MkOff day1 tid1 hdt1')
                    it "tests getting the full week" $ do
                        res <- runDB (weekGet week1)
                        let weekWithOneDayMaybe =
                                WeekF.add (MkHalfDayOff (MkOff day1 tid1 hdt1')) emptyWeek1
                        case weekWithOneDayMaybe of
                            Nothing -> expectationFailure "weekWithOneDayMaybe should not be Nothing"
                            Just week -> res `shouldBe` week
                    it "tests getting the full month" $ do
                        res <- runDB (monthGet month1)
                        let monthWithOneDayMaybe =
                                MonthF.add (MkHalfDayOff (MkOff day1 tid1 hdt1')) emptyMonth1
                        case monthWithOneDayMaybe of
                            Nothing -> expectationFailure "monthWithOneDayMaybe should not be Nothing"
                            Just month -> res `shouldBe` month
                    it "tests removing the entry" $ do
                        runDB (hdRm day1 tid1)
                        runDB (hdGet day1 tid1) `shouldThrow` hdNotFoundException
                    it "tests overriding with a worked entry"
                        $ runDB
                        $ projAdd project1 >> hdSetWorkDefault day1 tid1 project1
                    itemsNoWorkedEntry runDB
            context "When there is one work entry"
                $ before_ (runDB $ projAdd project1 >> hdSetWorkDefault day1 tid1 project1)
                $ do
                    it "tests getting the entry" $ do
                        worked <- runDB (hdGet day1 tid1)
                        worked `shouldBe` defaultWorked day1 tid1 project1
                    it "tests getting the full week" $ do
                        res <- runDB (weekGet week1)
                        let weekWithOneDayMaybe =
                                WeekF.add (defaultWorked day1 tid1 project1) emptyWeek1
                        case weekWithOneDayMaybe of
                            Nothing -> expectationFailure "weekWithOneDayMaybe should not be Nothing"
                            Just week -> res `shouldBe` week
                    it "tests getting the full month" $ do
                        res <- runDB (monthGet month1)
                        let monthWithOneDayMaybe =
                                MonthF.add (defaultWorked day1 tid1 project1) emptyMonth1
                        case monthWithOneDayMaybe of
                            Nothing -> expectationFailure "monthWithOneDayMaybe should not be Nothing"
                            Just month -> res `shouldBe` month
                    it "tests removing the entry" $ do
                        runDB (hdRm day1 tid1)
                        runDB (hdGet day1 tid1) `shouldThrow` hdNotFoundException
                    it "tests overriding with a work entry" $ do
                        runDB $ do
                            projAdd project2
                            hdSetWorkDefault day1 tid1 project2
                        worked <- runDB (hdGet day1 tid1)
                        worked `shouldBe` defaultWorked day1 tid1 project2
                    it "tests overriding with a day off" $ do
                        runDB $ hdSetOff day1 tid1 hdt1
                        res <- runDB (hdGet day1 tid1)
                        res `shouldBe` MkHalfDayOff (MkOff day1 tid1 hdt1)
                    it "tests setting arrived time" $ do
                        runDB (hdSetArrived day1 tid1 (arrived1 tid1))
                        mbWorked <- toMbWorked <$> runDB (hdGet day1 tid1)
                        mbWorked `hdwShouldSatisfy` ((==) (arrived1 tid1) . _workedArrived)
                    it "tests setting left time" $ do
                        runDB (hdSetLeft day1 tid1 (left1 tid1))
                        mbWorked <- toMbWorked <$> runDB (hdGet day1 tid1)
                        mbWorked `hdwShouldSatisfy` ((==) (left1 tid1) . _workedLeft)
                    it "tests setting arrived and left time" $ do
                        runDB (hdSetArrivedAndLeft day1 tid1 (arrived1 tid1) (left1 tid1))
                        mbWorked <- toMbWorked <$> runDB (hdGet day1 tid1)
                        -- We check that arrived and left times are good
                        mbWorked
                            `hdwShouldSatisfy` ( and
                                                     . ap
                                                         [ (==) (arrived1 tid1) . _workedArrived,
                                                           (==) (left1 tid1) . _workedLeft
                                                         ]
                                                     . pure
                                               )
                    it "tests setting notes" $ do
                        runDB (hdSetNotes day1 tid1 notes1)
                        mbWorked <- toMbWorked <$> runDB (hdGet day1 tid1)
                        mbWorked `hdwShouldSatisfy` ((==) notes1 . _workedNotes)
                    it "tests setting office" $ do
                        runDB (hdSetOffice day1 tid1 office1)
                        mbWorked <- toMbWorked <$> runDB (hdGet day1 tid1)
                        mbWorked `hdwShouldSatisfy` ((==) office1 . _workedOffice)
                    it "tests setting the project" $ do
                        runDB (projAdd project2 >> hdSetProject day1 tid1 project2)
                        mbWorked <- toMbWorked <$> runDB (hdGet day1 tid1)
                        mbWorked `projShouldBe` project2
            context "Test properties" $ do
                it "prop_hdSetOff" $
                    property (prop_hdSetOff runDB)
                it "prop_hdSetWorkNoProj" $
                    property (prop_hdSetWorkNoProj runDB)
                it "prop_hdSetWork" $
                    property (prop_hdSetWork runDB)
                it "prop_hdSetArrived" $
                    property (prop_hdSetArrived runDB)
                it "prop_hdSetLeft" $
                    property (prop_hdSetLeft runDB)
                it "prop_hdSetArrivedAndLeft" $
                    property (prop_hdSetArrivedAndLeft runDB)
                it "prop_hdSetNotes" $
                    property (prop_hdSetNotes runDB)
                it "prop_hdSetOffice" $
                    property (prop_hdSetOffice runDB)
                it "prop_hdSetProject" $
                    property (prop_hdSetProject runDB)
    where
        projShouldBe mbHdwProj proj = mbHdwProj `shouldSatisfy` checkProj proj
        hdwShouldSatisfy mbHdwProj pred = mbHdwProj `shouldSatisfy` maybe False pred

-- | Test the user API
testUserApi :: RunDB -> Spec
testUserApi runDB =
    describe "Test the user API" $ do
        context "When the DB is empty" $ do
            it "tests the uniqueness of the login" $
                runDB
                    ( userAdd user1 password1 defaultCost
                          >> userAdd user1 password1 defaultCost
                    )
                    `shouldThrow` userExistsException
            it "tests if a user does not exists" $
                runDB (userExists user1) `shouldReturn` False
            it "tests if we can remove a user" $
                runDB (userRm user1) `shouldThrow` userNotFoundException
            it "tests if we can rename a user not present in the db" $
                runDB (userRename user1 user2)
                    `shouldThrow` userNotFoundException
            it "tests if we can check the password of a user not present in the db" $
                runDB (userCheck user1 password1)
                    `shouldThrow` userNotFoundException
            it "tests if we can change the password of a user not present in the db" $
                runDB (userChangePassword user1 password1 defaultCost)
                    `shouldThrow` userNotFoundException
            it "tests the list of users" $
                runDB userList `shouldReturn` []
        context "One user in the DB"
            $ before_ (runDB (userAdd user1 password1 defaultCost))
            $ after_ (runDB cleanDB)
            $ do
                it "tests if the user exists" $
                    runDB (userExists user1) `shouldReturn` True
                it "tests if we can remove the user" $ do
                    exists <- runDB $ do
                        userRm user1
                        userExists user1
                    exists `shouldBe` False
                it "tests if we can rename it" $ do
                    runDB $ userRename user1 user2
                    user1Exists <- runDB (userExists user1)
                    user1Exists `shouldBe` False
                    user2Exists <- runDB (userExists user2)
                    user2Exists `shouldBe` True
                it "check a good password" $
                    runDB (userCheck user1 password1) `shouldReturn` True
                it "check a wrong password" $
                    runDB (userCheck user1 password2) `shouldReturn` False
                it "tests password change" $ do
                    runDB (userChangePassword user1 password2 defaultCost)
                    runDB (userCheck user1 password2) `shouldReturn` True
                it "tests the list of users" $
                    runDB userList `shouldReturn` [user1]
        context "Test properties" $ do
            it "userAdd userExists" $
                property (prop_userAddUserExists runDB)
            it "userAdd userAdd" $
                property (prop_userAddUserAdd runDB)
            it "userList" $
                property (prop_userList runDB)

-- | Main function
main :: IO ()
main = runNoLoggingT . withSqliteConn ":memory:" $ \conn -> liftIO $ do
    -- Setup a run function which captures the connection
    let runDB :: RunDB
        runDB actions = runSqlPersistM actions conn
    -- Launch the tests
    hspec $ beforeAll (runDB $ runMigration migrateAll) $ do
        testProjApi runDB
        testHdApi runDB
        testUserApi runDB
