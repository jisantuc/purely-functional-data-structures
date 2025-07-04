module LeftistHeap where

data Heap a = Node a (Heap a) (Heap a) Int | Empty deriving (Eq, Show)

empty :: Heap a
empty = Empty

isEmpty :: Eq a => Heap a -> Bool
isEmpty = (== Empty)

rank :: Heap a -> Int
rank Empty = 0
rank (Node _ _ _ r) = r

singleton :: a -> Heap a
singleton x = Node x Empty Empty 1

isLeftist :: Heap a -> Bool
isLeftist Empty = True
isLeftist (Node _ left right _) =
  rank left >= rank right && isLeftist left && isLeftist right

makeT :: a -> Heap a -> Heap a -> Heap a
makeT x left right =
  if (rank left >= rank right) then Node x left right (rank right + 1)
  else Node x right left (rank left + 1)

-- giving a semigroup and monoid instance instead of defining merge
instance Ord a => Semigroup (Heap a) where
  Empty <> h = h
  h <> Empty = h
  h1@(Node x ll lr _) <> h2@(Node y rl rr _) =
    if (x <= y) then makeT x ll (lr <> h2) else
      makeT y rl (rr <> h1)

instance Ord a => Monoid (Heap a) where
  mempty = Empty

insert :: Ord a => a -> Heap a -> Heap a
insert x h =
  Node x Empty Empty 1 <> h

-- Exercise 3.2: Define insert directly rather than via a call to merge
insert' :: (Show a, Ord a) => a -> Heap a -> Heap a
insert' x Empty = singleton x
insert' x (Node y left@(Node z _ _ _) right r) =
  if (x <= y) then (Node x (insert' y left) right (r +1)) else
    if (x <= z) then Node y (insert' x left) right r else
      Node y left (insert' x right) r
insert' x (Node y Empty right r) =
  if x < y then
    Node x (singleton y) right r
  else
    Node y (singleton x) right r

-- Exercise 3.3: Define a function fromList of type [a] -> Heap a that produces a
-- leftist heap from an unordered list of elements. Instead of merging the heaps in one pass,
-- merge pairs of them (?)
fromList :: Ord a => [a] -> Heap a
fromList =
  concatHeaps . (singleton <$>)

concatHeaps :: Ord a => [Heap a] -> Heap a
concatHeaps [] = Empty
concatHeaps heaps =
  case mergePairs heaps of
    [h] ->
      h
    mergedPairs ->
      concatHeaps mergedPairs

mergePairs :: Ord a => [Heap a] -> [Heap a]
mergePairs [] = []
mergePairs (x:[]) = [x]
mergePairs (x1:x2:xs) = [x1 <> x2] ++ mergePairs xs


findMin :: Ord a => Heap a -> Maybe a
findMin Empty = Nothing
findMin (Node x _ _ _ ) = Just x

deleteMin :: Ord a => Heap a -> Heap a
deleteMin Empty = Empty
deleteMin (Node _ l r _) = l <> r
