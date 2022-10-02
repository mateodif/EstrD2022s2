module SetRepetido where

data Set a = S [a] Int
  deriving (Show)

-- Crea un conjunto vacío.
emptyS :: Set a
emptyS = S [] 0

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece a (x : xs) = a == x || pertenece a xs

-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
contarUnicos :: Eq a => [a] -> Int
contarUnicos [] = 0
contarUnicos (x : xs) =
  (if pertenece x xs then 1 else 0) + contarUnicos xs

addS :: Eq a => a -> Set a -> Set a
addS a (S xs l) =
  let agregado = a : xs
   in S agregado (contarUnicos agregado)

-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs :: Eq a => a -> Set a -> Bool
belongs a (S xs _) = pertenece a xs

-- Devuelve la cantidad de elementos distintos de un conjunto.
sizeS :: Eq a => Set a -> Int
sizeS (S xs _) = contarUnicos xs

-- Borra un elemento del conjunto.
sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x : xs) =
  if n == x
    then xs
    else x : sacar n xs

removeS :: Eq a => a -> Set a -> Set a
removeS a (S xs l) =
  let sacado = sacar a xs
   in S sacado (contarUnicos sacado)

---------- todo lo de abajo es wip


-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unir :: Eq a => [a] -> [a] -> [a]
unir [] ys = ys
unir (x : xs) ys =
  if pertenece x ys
    then unir xs ys
    else x : unir xs ys

unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs _) (S ys _) =
  let union = unir xs ys
   in S union (length union)

-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList :: Eq a => Set a -> [a]
setToList (S xs _) = xs
