-- O(1)
head' :: [a] -> a
head' (x:xs) = x

-- O(1)
sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

-- O(n) donde n es el numero de factorial que estemos calculando
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- O(n) donde n es la cantidad de elementos de la lista
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- O(n) donde n es la cantidad de numeros en la lista
factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs

-- O(n) donde n es la cantidad de elementos en la lista
pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

-- O(n^2) donde n es la cantidad de elementos en la lista
-- (se llama una funcion lineal en la recursion)
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) =
  if pertenece x xs
    then sinRepetidos xs
    else x : sinRepetidos xs

-- equivalente a (++)
-- O(n) donde n es la primer lista
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

-- O(n^2) donde n es la cantidad de strings en la lista
-- (se llama al append por cada elemento en la recursion)
concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

-- O(n) donde n es la cantidad de elementos en la lista
takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

-- O(n) donde n es la cantidad de elementos en la lista
dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

-- O(n) donde n es la cantidad de elementos en la lista
-- (es lineal porque se estan haciendos dos operaciones
-- lineales de forma simultanea, 2n se simplifica a n)
partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)

-- O(n) donde n es la cantidad de elementos en la lista
-- (es lineal porque min es constante, se desprecia)
minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

-- O(n) donde n es la cantidad de elementos en la lista
sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) =
  if n == x
    then xs
    else x : sacar n xs

-- O(n^2) donde n es la cantidad de elementos en la lista
-- se realizan 3 operaciones, dos de las cuales se hacen
-- por cada recursion
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs =
  let m = minimo xs
  in m : ordenar (sacar m xs)
