module Test.Data.Okasaki.HeapSpec where

import Data.Foldable (Foldable (foldl'))
import Data.Functor ((<&>))
import Data.Okasaki.Heap
  ( LeftistHeap,
    empty,
    insert,
    isLeftist,
    merge,
    singleton,
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (arbitrary, sized, vectorOf)

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
      vectorOf 3 arbitrary <&> \ints ->
        let heaps :: [LeftistHeap Int]
            heaps = singleton <$> ints
            heap = foldl' merge empty heaps
         in isLeftist heap
  describe "insert" $ do
    it "inserts into empty heaps" $
      insert 3 (empty :: LeftistHeap Int) `shouldBe` singleton 3
    prop "inserts to leftist heaps" $ vectorOf 8 arbitrary <&> \ints ->
      let heaps :: [LeftistHeap Int]
          heaps = singleton <$> take 7 ints 
          baseHeap = foldl' merge empty heaps
          finalHeap = (`insert` baseHeap) . (!! 7) $ ints
       in isLeftist finalHeap
