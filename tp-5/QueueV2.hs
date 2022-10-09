module QueueV2 where

data Queue a = Q [a] [a]
  deriving (Show)

-- emptyQ = Q [] []
-- queue 1 = Q [1] []
-- queue 2 = Q [1] [2]
-- queue 3 = Q [1] [3, 2]
-- queue 4 = Q [1] [4, 3, 2]
-- dequeue = Q [2, 3, 4] []
-- queue 5 = Q [2, 3, 4] [5]

-- Crea una cola vacía.
emptyQ :: Queue a
emptyQ = Q [] []

-- Dada una cola indica si la cola está vacía.
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q [] []) = True
isEmptyQ _ = False

-- Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue :: a -> Queue a -> Queue a
enqueue a (Q [] []) = Q [a] []
enqueue a (Q l l2) = Q l (a : l2)

-- Dada una cola devuelve el primer elemento de la cola.
firstQ :: Queue a -> a
firstQ (Q (x:_) _) = x

-- Dada una cola la devuelve sin su primer elemento.
dequeue :: Queue a -> Queue a
dequeue (Q [] ys) = Q (reverse ys) []
dequeue (Q (x:xs) ys) = Q (reverse ys) xs
