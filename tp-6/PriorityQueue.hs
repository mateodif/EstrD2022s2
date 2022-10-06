module PriorityQueue where

data PriorityQueue a = PQ [a]
  deriving Show

-- O(1)
emptyPQ :: PriorityQueue a
emptyPQ = PQ []

-- O(1)
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (PQ []) = True
isEmptyPQ _ = False

-- O(1)
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ a (PQ l) = PQ (a : l)

-- O(n)
findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (PQ l) = minimum l

-- O(n)
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete a (x:xs) = if a == x then xs else x : delete a xs

-- O(n)
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (PQ l) = PQ (delete (minimum l) l)
