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
-- O(n^2) donde n es la cantidad de keys del map
-- el costo de lookupM es lineal y se hace en cada llamado recursivo
getFromKeys :: Eq k => [k] -> Map k v -> [Maybe v]
getFromKeys []     m = []
getFromKeys (k:ks) m = lookupM k m : getFromKeys ks m

-- O(n^2) donde n es la cantidad de keys del map
-- se esta llamando a la funcion getFromKeys que es O(n^2) y ademas
-- se llama a keys que es O(n), pero al realizarse "al mismo tiempo"
-- seria O(n^2 + n) => O(n^2)
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m = getFromKeys (keys m) m

-- Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas [] m = True
todasAsociadas (k:ks) m =
  case lookupM k m of
    Nothing -> False
    _ -> todasAsociadas ks m


-- Propósito: convierte una lista de pares clave valor en un map.
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap []       = emptyM
listToMap (kv:kvs) =
  let (k, v) = kv in
    assocM k v (listToMap kvs)
