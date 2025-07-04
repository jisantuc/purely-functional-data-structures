module Test.Data.Okasaki.BinaryTreeSpec where

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Data.Okasaki.BinaryTree (member, empty, insert, singleton)

spec :: Spec
spec = describe "BinaryTreeSpec" $ do
  prop "doesn't find elements in empty trees" $ \x ->
    not $ member empty (x :: Int)
  prop "finds elements in singletons of those elements" $ \x ->
    member (singleton x) (x :: Char)
  prop "finds elements inserted into larger trees" $ \(x, y, z) ->
    let bigTree = insert (insert (singleton x) y) z
     in member bigTree (z :: Int)
