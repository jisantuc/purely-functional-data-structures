module Data.Okasaki.BinaryTree (BinaryTree, empty, insert, member, singleton) where

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Show)

singleton :: Ord a => a -> BinaryTree a
singleton a = Node Leaf a Leaf

empty :: Ord a => BinaryTree a
empty = Leaf

member :: (Ord a) => BinaryTree a -> a -> Bool
member Leaf _ = False
member (Node left value right) a = case compare a value of
  EQ -> True
  LT -> member left a
  GT -> member right a

insert :: (Ord a) => BinaryTree a -> a -> BinaryTree a
insert Leaf a = singleton a
insert tree@(Node left value right) a = case compare a value of
  EQ -> tree
  LT -> Node (left `insert` a) value right
  GT -> Node left value $ right `insert` a
