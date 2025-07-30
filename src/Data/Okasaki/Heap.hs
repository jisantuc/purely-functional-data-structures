{-# LANGUAGE LambdaCase #-}

module Data.Okasaki.Heap
  ( LeftistHeap,
    deleteMin,
    empty,
    findMin,
    fromList,
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

fromList :: (Ord a) => [a] -> LeftistHeap a
fromList [] = empty
fromList [x] = singleton x
fromList [x, y] = singleton x `merge` singleton y
fromList xs =
  go . pairs $ singleton <$> xs
  where
    pairs [] = []
    pairs [x] = [(x, Nothing)]
    pairs [x, y] = [(x, Just y)]
    pairs (x : y : t) = [(x, Just y)] <> pairs t
    go [(h, Nothing)] = h
    go [(h, Just h')] = h `merge` h'
    go [] = empty
    go hs =
      go . pairs $
        ( \case
            (h, Nothing) -> h
            (h, Just h') -> h `merge` h'
        )
          <$> hs

insert :: (Ord a) => a -> LeftistHeap a -> LeftistHeap a
insert v Leaf = singleton v
insert v h@(Node rank' v' l r) =
  if v <= v'
    then Node (rank' + 1) v h Leaf
    else
      let inserted = insert v r
       in if rank inserted > rank l
            then Node rank' v' inserted l
            else Node rank' v' l inserted

merge :: (Ord a) => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
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
findMin Leaf = Nothing
findMin (Node _ v _ _) = Just v

deleteMin :: (Ord a) => LeftistHeap a -> LeftistHeap a
deleteMin Leaf = Leaf
deleteMin (Node _ _ l r) = merge l r

rank :: LeftistHeap a -> Int
rank Leaf = 0
rank (Node r _ _ _) = r

rankLeft :: LeftistHeap a -> Int
rankLeft Leaf = 0
rankLeft (Node _ _ Leaf _) = 1
rankLeft (Node _ _ (Node r _ _ _) _) = r

isLeftist :: (Show a) => LeftistHeap a -> Bool
isLeftist Leaf = True
isLeftist h@(Node _ _ l r) =
  rankLeft h >= rank r && isLeftist l && isLeftist r
