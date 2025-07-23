module Test.Data.Okasaki.HeapSpec where

import Data.Foldable (Foldable (foldl'))
import Data.Functor ((<&>))
import Data.Okasaki.Heap
  ( LeftistHeap,
    deleteMin,
    empty,
    findMin,
    insert,
    isLeftist,
    merge,
    singleton,
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (arbitrary, vectorOf)

spec :: Spec
spec = describe "HeapSpec" $ do
  describe "merge" $ do
    it "merges empty with empty heaps" $
      (empty :: LeftistHeap Int) `merge` empty `shouldBe` empty
    prop "mergest empty heaps with singleton heaps" $
      \c -> empty `merge` singleton c `shouldBe` singleton (c :: Char)
    it "merges to leftist heaps for a known list" $
      let heaps = singleton <$> [2, 1, 0 :: Integer]
          heap = foldl' merge empty heaps
       in isLeftist heap `shouldBe` True
    prop "merges to leftist heaps" $
      vectorOf 100 arbitrary <&> \ints ->
        let heaps :: [LeftistHeap Int]
            heaps = singleton <$> ints
            heap = foldl' merge empty heaps
         in isLeftist heap
  describe "insert" $ do
    it "inserts into empty heaps" $
      insert 3 (empty :: LeftistHeap Int) `shouldBe` singleton 3
    prop "inserts to leftist heaps" $
      vectorOf 100 arbitrary <&> \ints ->
        let heaps :: [LeftistHeap Int]
            heaps = singleton <$> take 99 ints
            baseHeap = foldl' merge empty heaps
            finalHeap = (`insert` baseHeap) . (!! 99) $ ints
         in isLeftist finalHeap
  describe "deleteMin" $
    it "deletes the min" $
      let initialHeap = foldl' merge empty $ singleton <$> [1, 2, 3, 4 :: Integer]
          expectedHeap = foldl' merge empty $ singleton <$> [2, 3, 4]
          finalHeap = deleteMin initialHeap
       in findMin finalHeap `shouldBe` findMin expectedHeap
