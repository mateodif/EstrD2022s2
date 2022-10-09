module Queue where

data Queue a = Q [a]
  deriving Show

-- Una Queue es un tipo abstracto de datos de naturaleza FIFO (first in, first out). Esto significa
-- que los elementos salen en el orden con el que entraron, es decir, el que se agrega primero es el
-- primero en salir (como la cola de un banco).

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
-- O(n)
enqueue :: a -> Queue a -> Queue a
enqueue a (Q l) = Q (l ++ [a])

-- Dada una cola devuelve el primer elemento de la cola.
-- O(1)
firstQ :: Queue a -> a
firstQ (Q l) = head l

-- Dada una cola la devuelve sin su primer elemento.
-- O(1)
dequeue :: Queue a -> Queue a
dequeue (Q l) = Q (tail l)
