import Map (Map, assocM, deleteM, emptyM, keys, lookupM)
import PriorityQueue (PriorityQueue, deleteMinPQ, emptyPQ, findMinPQ, insertPQ, isEmptyPQ)

listToPQ :: Ord a => [a] -> PriorityQueue a
listToPQ [] = emptyPQ
listToPQ (x : xs) = insertPQ x (listToPQ xs)

pqToList :: Ord a => PriorityQueue a -> [a]
pqToList pq =
  if isEmptyPQ pq
    then []
    else findMinPQ pq : pqToList (deleteMinPQ pq)

heapSort :: Ord a => [a] -> [a]
heapSort l = pqToList (listToPQ l)

------------------------------------------------

valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m =
  let ks = keys m in
