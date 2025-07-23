module Data.Okasaki.Heap
  ( LeftistHeap,
    deleteMin,
    empty,
    findMin,
    insert,
    isEmpty,
    isLeftist,
    merge,
    singleton,
  )
where

data LeftistHeap a
  = Leaf
  | Node Int a (LeftistHeap a) (LeftistHeap a)
  deriving (Eq, Show)

empty :: LeftistHeap a
empty = Leaf

isEmpty :: (Ord a) => LeftistHeap a -> Bool
isEmpty = (== empty)

singleton :: a -> LeftistHeap a
singleton v = Node 1 v Leaf Leaf

insert :: (Ord a) => a -> LeftistHeap a -> LeftistHeap a
insert v Leaf = singleton v
insert v h@(Node rank' v' l r) =
  if v <= v'
    then Node (rank' + 1) v h Leaf
    else Node rank' v' l $ insert v r

merge :: (Show a, Ord a) => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
merge Leaf h = h
merge h Leaf = h
merge h1@(Node _ x a1 b1) h2@(Node _ y a2 b2) =
  if x <= y
    then
      makeHeap x a1 (merge b1 h2)
    else
      makeHeap y a2 (merge h1 b2)
  where
    makeHeap x' a b =
      if rank a >= rank b
        then Node (rank b + 1) x' a b
        else Node (rank a + 1) x' b a

findMin :: LeftistHeap a -> Maybe a
findMin = undefined

deleteMin :: LeftistHeap a -> LeftistHeap a
deleteMin = undefined

{-# INLINE rank #-}
rank :: LeftistHeap a -> Int
rank Leaf = 0
rank (Node r _ _ _) = r

rankLeft :: LeftistHeap a -> Int
rankLeft Leaf = 0
rankLeft (Node _ _ Leaf _) = 1
rankLeft (Node _ _ (Node r _ _ _) _) = r

isLeftist :: (Show a) => LeftistHeap a -> Bool
isLeftist Leaf = True
isLeftist h@(Node rank' _ l r) =
  rankLeft h >= rank' && isLeftist l && isLeftist r
