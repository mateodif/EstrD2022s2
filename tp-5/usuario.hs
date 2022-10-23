import Queue (Queue, dequeue, emptyQ, enqueue, firstQ, isEmptyQ)
import Set
  ( Set,
    addS,
    belongs,
    emptyS,
    removeS,
    setToList,
    sizeS,
    unionS,
  )
import Stack (Stack, emptyS, isEmptyS, lenS, pop, push, top)

data Tree a
  = EmptyT
  | NodeT a (Tree a) (Tree a)
  deriving (Show)

-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
-- al conjunto.
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] _ = []
losQuePertenecen (x : xs) s =
  if belongs x s
    then x : losQuePertenecen xs s
    else losQuePertenecen xs s

-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidosSet :: Eq a => [a] -> Set a
sinRepetidosSet [] = Set.emptyS
sinRepetidosSet (x : xs) = addS x (sinRepetidosSet xs)

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos l = setToList (sinRepetidosSet l)

-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
-- del arbol.
arbolConSets :: Tree (Set String)
arbolConSets =
  NodeT
    (addS "a" Set.emptyS)
    (NodeT (addS "b" Set.emptyS) EmptyT EmptyT)
    EmptyT

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = Set.emptyS
unirTodos (NodeT s t1 t2) = unionS s (unionS (unirTodos t1) (unirTodos t2))

-----------------------------------------

lengthQ :: Queue a -> Int
lengthQ q = if isEmptyQ q then 0 else 1 + lengthQ (dequeue q)

queueToList :: Queue a -> [a]
queueToList q =
  if isEmptyQ q then [] else firstQ q : queueToList (dequeue q)

unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 =
  if isEmptyQ q2 then q1 else enqueue (firstQ q2) (unionQ q1 (dequeue q2))

-------------------------------------------

apilar :: [a] -> Stack a
apilar [] = Stack.emptyS
apilar (x:xs) = push x (apilar xs)

-- Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar :: Stack a -> [a]
desapilar s =
  if isEmptyS s then [] else top s : desapilar (pop s)

-- Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
-- posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos p e s =
  if p == 0
    then push e s
    else push (top s) (insertarEnPos (p-1) e (pop s))
