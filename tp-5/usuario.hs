import Set ()

-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
-- al conjunto.
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen = undefined

-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos = undefined

-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
-- del arbol.
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos = undefined
