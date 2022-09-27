module Set where

data Set a = S [a] Int

-- Crea un conjunto vacío.
emptyS :: Set a
emptyS = S [] 0

-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS :: Eq a => a -> Set a -> Set a
addS a (S xs l) = S (a : xs) (l + 1)

-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece a (x:xs) = a == x || pertenece a xs

belongs :: Eq a => a -> Set a -> Bool
belongs a (S xs _) = pertenece a xs

-- Devuelve la cantidad de elementos distintos de un conjunto.
cantidadDeUnicos :: Eq a => [a] -> Int
cantidadDeUnicos [] = 0
cantidadDeUnicos (x:xs) =
  if pertenece x xs
    then cantidadDeUnicos xs
    else 1 + cantidadDeUnicos xs

sizeS :: Eq a => Set a -> Int
sizeS (S xs _) = cantidadDeUnicos xs

-- Borra un elemento del conjunto.
sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) =
  if n == x
    then xs
    else x : sacar n xs

removeS :: Eq a => a -> Set a -> Set a
removeS a (S xs l) = S (sacar a xs) (l - 1)

-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
appendSinRepetidos :: Eq a => [a] -> [a] -> [a]
appendSinRepetidos [] ys = ys
appendSinRepetidos (x:xs) ys =
  if pertenece x ys
    then appendSinRepetidos xs ys
    else x : appendSinRepetidos xs ys

unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs _) (S ys _) =
  let union = appendSinRepetidos xs ys
  in S union (cantidadDeUnicos union)

-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList :: Eq a => Set a -> [a]
setToList (S xs _) = xs
