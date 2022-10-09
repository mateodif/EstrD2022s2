module QueueReverse where

data Queue a = Q [a]
  deriving Show

-- Implemente ahora la versión que agrega por delante y quita por el final de la lista. Compare
-- la eficiencia entre ambas implementaciones.


-- Crea una cola vacía.
-- n(1)
emptyQ :: Queue a
emptyQ = Q []

-- Dada una cola indica si la cola está vacía.
-- n(1)
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q []) = True
isEmptyQ _      = False

-- Dados un elemento y una cola, agrega ese elemento a la cola.
-- O(1)
enqueue :: a -> Queue a -> Queue a
enqueue a (Q l) = Q (a:l)

-- Dada una cola devuelve el primer elemento de la cola.
-- O(n)
firstQ :: Queue a -> a
firstQ (Q l) = last l

-- Dada una cola la devuelve sin su primer elemento.
-- O(n)
dequeue :: Queue a -> Queue a
dequeue (Q l) = Q (init l)
