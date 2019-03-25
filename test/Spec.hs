import           RIO
import qualified RIO.List.Partial as L (head) 

import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ 
  describe "RIO.List.Partial.head" $ do
    it "returns the first element of a list" $ 
      L.head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> L.head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $
      evaluate (L.head []) `shouldThrow` anyException