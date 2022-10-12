module QueueReverse where

data Queue a = Q [a]
  deriving Show

-- Implemente ahora la versión que agrega por delante y quita por el final de la lista. Compare
-- la eficiencia entre ambas implementaciones.


-- Crea una cola vacía.
-- O(1) porque la operacion es constante
emptyQ :: Queue a
emptyQ = Q []

-- Dada una cola indica si la cola está vacía.
-- O(1) porque la operacion (comparacion por pattern matching) es constante
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q []) = True
isEmptyQ _      = False

-- Dados un elemento y una cola, agrega ese elemento a la cola.
-- O(1) porque el costo de agregar un elemento al principio de la lista es constante
enqueue :: a -> Queue a -> Queue a
enqueue a (Q l) = Q (a:l)

-- Dada una cola devuelve el primer elemento de la cola.
-- O(n) donde n es la cantidad de elementos en la lista
-- es lineal porque se debe atravesar toda la lista para devolver el ultimo elemento
firstQ :: Queue a -> a
firstQ (Q l) = last l

-- Dada una cola la devuelve sin su primer elemento.
-- O(n) donde n es la cantidad de elementos en la lista
-- es lineal porque se debe atravesar toda la lista para devolver todo menos el primer elemento
dequeue :: Queue a -> Queue a
dequeue (Q l) = Q (init l)
