import qualified LeftistHeap as LH

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "fromList creates legitimate leftist trees" $
    do
      it "prop_isLeftist" $ property prop_isLeftist
  describe "insert and insert' yield the same results" $
    do
      it "prop_insertsMatch" $ property prop_insertsMatch

newtype TestLH a = TestLH (LH.Heap a) deriving (Show)

instance (Arbitrary a, Ord a) => Arbitrary (TestLH a) where
  arbitrary =
    do
      vals <- LH.fromList <$> arbitrary
      oneof [(pure . TestLH $ LH.Empty), (pure . TestLH $ vals)]

-- test whether insert defined directly has the same results as the merge strategy
prop_insertsMatch :: Int -> TestLH Int -> Bool
prop_insertsMatch val (TestLH tree) =
  LH.insert val tree == LH.insert' val tree

-- test whether fromList is actually making leftist heaps
prop_isLeftist :: TestLH Int -> Bool
prop_isLeftist (TestLH LH.Empty) = True
prop_isLeftist (TestLH node) =
  LH.isLeftist node
