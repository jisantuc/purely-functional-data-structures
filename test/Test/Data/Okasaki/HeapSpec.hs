module Test.Data.Okasaki.HeapSpec where

import Data.Foldable (Foldable (foldl'))
import Data.Functor ((<&>))
import Data.Okasaki.Heap (LeftistHeap, empty, isLeftist, merge, singleton)
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
    -- oh dang these tests look like they're failing for good reasons, what have I done wrong?
    -- diagram it out on paper, since the show instance for LeftistHeaps is hard to parse
    it "merges to leftist heaps for a known list" $
      let heaps = singleton <$> [2, 1, 0 :: Integer]
          heap = foldl' merge empty heaps
       in isLeftist heap `shouldBe` True
    prop "merges to leftist heaps" $
      vectorOf 2 arbitrary <&> \ints ->
        let heaps :: [LeftistHeap Int]
            heaps = singleton <$> ints
            heap = foldl' merge empty heaps
         in isLeftist heap
