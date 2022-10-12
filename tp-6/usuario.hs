import Map (Map, assocM, deleteM, emptyM, keys, lookupM)
import PriorityQueue (PriorityQueue, deleteMinPQ, emptyPQ, findMinPQ, insertPQ, isEmptyPQ)

-- O(n) donde n es la cantidad de elementos en la lista
-- es lineal porque debe atravesar todos los elementos
-- de la lista y agregarlos a un PQ (agregar es O(1))
listToPQ :: Ord a => [a] -> PriorityQueue a
listToPQ [] = emptyPQ
listToPQ (x : xs) = insertPQ x (listToPQ xs)

-- O(n^2) donde n es la cantidad de elementos de la priority
-- es cuadratica porque por cada elemento debe realizar dos
-- operaciones lineales
-- es decir, esta haciendo recursion y ademas en cada iteracion
-- llama a una funcion lineal
pqToList :: Ord a => PriorityQueue a -> [a]
pqToList pq =
  if isEmptyPQ pq
    then []
    else findMinPQ pq : pqToList (deleteMinPQ pq)

-- O(n)
heapSort :: Ord a => [a] -> [a]
heapSort l = pqToList (listToPQ l)

------------------------------------------------

-- Proposito: obtiene los valores asociados a cada clave del map.
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m =
  let ks = keys m in
