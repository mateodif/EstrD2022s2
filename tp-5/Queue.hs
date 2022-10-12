module Queue where

data Queue a = Q [a]
  deriving Show

-- Una Queue es un tipo abstracto de datos de naturaleza FIFO (first in, first out). Esto significa
-- que los elementos salen en el orden con el que entraron, es decir, el que se agrega primero es el
-- primero en salir (como la cola de un banco).

-- Crea una cola vacía.
-- O(1) ya que la operacion es fija, no crece segun sus parametros (en este caso no tiene)
emptyQ :: Queue a
emptyQ = Q []

-- Dada una cola indica si la cola está vacía.
-- O(1) ya que la operacion es fija, no crece segun sus parametros
-- en este caso es un pattern matching de igualdad
isEmptyQ :: Queue a -> Bool
isEmptyQ (Q []) = True
isEmptyQ _      = False

-- Dados un elemento y una cola, agrega ese elemento a la cola.
-- O(n) donde n es la cantidad de elementos de la lista (l)
-- es de costo lineal ya que para agregar un elemento al final de la lista
-- se debe atravesar toda la lista
enqueue :: a -> Queue a -> Queue a
enqueue a (Q l) = Q (l ++ [a])

-- Dada una cola devuelve el primer elemento de la cola.
-- O(1) ya que head es una operacion de costo constante
firstQ :: Queue a -> a
firstQ (Q l) = head l

-- Dada una cola la devuelve sin su primer elemento.
-- O(1) ya que tail es una operacion de costo constante
dequeue :: Queue a -> Queue a
dequeue (Q l) = Q (tail l)
