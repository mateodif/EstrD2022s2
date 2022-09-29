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

data Tree a
  = EmptyT
  | NodeT a (Tree a) (Tree a)
  deriving (Show)

-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
-- al conjunto.
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen []     _ = []
losQuePertenecen (x:xs) s =
  if belongs x s
    then x : losQuePertenecen xs s
    else losQuePertenecen xs s

-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidosSet :: Eq a => [a] -> Set a
sinRepetidosSet [] = emptyS
sinRepetidosSet (x:xs) = addS x (sinRepetidosSet xs)

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos l = setToList (sinRepetidosSet l)

-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
-- del arbol.
arbolConSets :: Tree (Set String)
arbolConSets = NodeT (addS "a" emptyS)
                (NodeT (addS "b" emptyS) EmptyT EmptyT)
                EmptyT

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT          = emptyS
unirTodos (NodeT s t1 t2) = unionS s (unionS (unirTodos t1) (unirTodos t2))
