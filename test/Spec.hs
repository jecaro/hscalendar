import           RIO
import qualified RIO.List.Partial as L (head) 

import           Database.Persist.Sqlite
    ( runMigration
    , runSqliteInfo 
    , mkSqliteConnectionInfo
    )
import           Control.Monad.Logger (runNoLoggingT)

import           Test.Hspec
import           Test.QuickCheck

import           Model
import           ModelFcts
    ( projAdd
    , projExists
    , )


main :: IO ()
main = hspec $ do

  describe "RIO.List.Partial.head" $ do
    it "returns the first element of a list" $ 
      L.head [23 ..] `shouldBe` (23 :: Int)
  
    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> L.head (x:xs) == (x :: Int)
  
    it "throws an exception if used with an empty list" $
      evaluate (L.head []) `shouldThrow` anyException
  
  describe "ModelFcts.projAdd" $ 
    it "tests adding a project" $ asIO $ runNoLoggingT . runSqliteInfo (mkSqliteConnectionInfo ":memory:") $ do
      runMigration migrateAll
      let proj = Project "test" 
      projAdd proj
      exists <- projExists proj
      liftIO $ exists `shouldBe` True
