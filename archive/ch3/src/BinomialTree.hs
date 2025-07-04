module BinomialTree where

import Data.List (uncons)
import Data.Semigroup

-- trees have some parameterized data type, a rank, and children of the same type
data Tree a = Node Int a (Heap a) deriving (Eq, Show)

-- heaps are collections of trees
type Heap a = [Tree a]

instance Ord a => Semigroup (Tree a) where
  t1@(Node r1 x1 c1) <> t2@(Node _ x2 c2) =
    if x1 <= x2 then
      Node (r1 + 1) x1 (t2 : c1)
    else
      Node (r1 + 1) x2 (t1 : c2)

singleton :: a -> Tree a
singleton x = Node 0 x []

-- linking draws a line from this tree to its child
link :: Ord a => Tree a -> Tree a -> Tree a
link = (<>)

rank :: Tree a -> Int
rank (Node r _ _) = r

root :: Tree a -> a
root (Node _ x _) = x

-- insTree, but with a non-stupid name
insert :: Ord a => Tree a -> Heap a -> Heap a
insert t [] = [t]
insert tree heap@(h:t) =
  if rank tree < rank h then
    tree : heap
  else
    insert (tree `link` h) t

insert' :: Ord a => a -> Heap a -> Heap a
insert' x h = insert (singleton x) h

-- if the rank of the head of the first heap is less than the rank of the head of the second heap,
-- the first heap's head wins, then merge the tail of the first heap with the whole second heap
-- otherwise do the opposite
-- but if they're equal, link them, and insert the link into the merged tails
merge :: Ord a => Heap a -> Heap a -> Heap a
merge [] heap2 = heap2
merge heap1 [] = heap1
merge heap1@(h1:t1) heap2@(h2:t2) =
  if rank h1 < rank h2 then
    h1 : (merge t1 heap2)
  else if rank h2 < rank h1 then
    h2 : (merge t2 heap1)
  else
    insert (link h1 h2) (merge t1 t2)

removeMinTree :: Ord a => Heap a -> Maybe (Tree a, Heap a)
removeMinTree [] = Nothing
removeMinTree heap@[t] = uncons heap
removeMinTree heap@(h:t) =
  let
    unconsedTail = removeMinTree t
  in
    case unconsedTail of
      Just (h', t') ->
        if root h < root h' then
          uncons heap
        else
          Just (h', h : t')

findMin :: Ord a => Heap a -> Maybe a
findMin [] = Nothing
findMin hs = case removeMinTree hs of
  Just (t, _) ->
    Just $ root t
  Nothing ->
    Nothing
