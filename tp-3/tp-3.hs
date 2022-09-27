data Color = Azul | Rojo
  deriving Show

data Celda = Bolita Color Celda | CeldaVacia
  deriving Show

celda0 = CeldaVacia
celde1 = Bolita Rojo CeldaVacia
celde2 = Bolita Rojo (Bolita Azul CeldaVacia)
celda3 = Bolita Rojo (Bolita Rojo  CeldaVacia)

coloresDeBolitasEnCelda :: Celda -> [Color]
coloresDeBolitasEnCelda CeldaVacia     = []
coloresDeBolitasEnCelda (Bolita co ce) = co : coloresDeBolitasEnCelda ce

unoSi :: Bool -> Int
unoSi b = if b then 1 else 0

esMismoColor :: Color -> Color -> Bool
esMismoColor Rojo Rojo = True
esMismoColor Azul Azul = True
esMismoColor _    _    = False

sumatoriaSegunColor :: Color -> [Color] -> Int
sumatoriaSegunColor _ []     = 0
sumatoriaSegunColor c (x:xs) = (if esMismoColor c x then 1 else 0) + sumatoriaSegunColor c xs

-- Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
-- existe una operación sobre listas que ayude a resolver el problema.
nroBolitas :: Color -> Celda -> Int
nroBolitas _  CeldaVacia = 0
nroBolitas co (Bolita co2 ce) = (if esMismoColor co co2 then 1 else 0) + nroBolitas co ce

-- Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner = Bolita

-- Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
-- Gobstones, esta función es total.
sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia = CeldaVacia
sacar co1 (Bolita co2 ce) =
  if esMismoColor co1 co2
    then ce
    else Bolita co2 (sacar co1 ce)

-- Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _  ce = ce
ponerN n co ce = ponerN (n - 1) co (poner co ce)

------------------------------------------------------

data Objeto = Cacharro | Tesoro
  deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
  deriving Show

tesoro0 = Fin
tesoro1 = Nada (Nada Fin)
tesoro2 = Nada (Nada (Nada (Cofre [Cacharro] Fin)))
tesoro3 = Nada (Nada (Nada (Cofre [Tesoro] (Nada Fin))))

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

hayTesoroEnCofre :: [Objeto] -> Bool
hayTesoroEnCofre []     = False
hayTesoroEnCofre (x:xs) = esTesoro x || hayTesoroEnCofre xs

-- Indica si hay un cofre con un tesoro en el camino.
hayTesoro :: Camino -> Bool
hayTesoro Fin            = False
hayTesoro (Nada c)       = hayTesoro c
hayTesoro (Cofre objs c) = hayTesoroEnCofre objs || hayTesoro c

-- Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
-- Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
-- Precondición: tiene que haber al menos un tesoro.
camino0 = Cofre [Tesoro] (Nada Fin)
camino1 = Nada (Nada (Nada (Cofre [Tesoro] (Nada Fin))))
camino2 = Nada (Cofre [Cacharro] (Nada (Cofre [Tesoro] (Cofre [Tesoro] Fin))))

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin            = 0
pasosHastaTesoro (Cofre objs c) = if hayTesoroEnCofre objs then 0 else 1 + pasosHastaTesoro c
pasosHastaTesoro (Nada c)       = 1 + pasosHastaTesoro c

-- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
-- pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n c = n == pasosHastaTesoro c

-- Indica si hay al menos “n” tesoros en el camino.
cantidadDeTesoros :: Camino -> Int
cantidadDeTesoros Fin = 0
cantidadDeTesoros (Cofre objs c) = (if hayTesoroEnCofre objs then 1 else 0) + cantidadDeTesoros c
cantidadDeTesoros (Nada c) = cantidadDeTesoros c

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n c = cantidadDeTesoros c > n

-- Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
-- el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
-- incluidos tanto 3 como 5 en el resultado.
-- cantTesorosEntre :: Int -> Int -> Camino -> Int


-- Dada esta definición para árboles binarios
data Tree a =
  EmptyT
  | NodeT a (Tree a) (Tree a)
  deriving Show

tree0 :: Tree Int
tree0 = NodeT 1 (NodeT 2 EmptyT EmptyT) EmptyT

tree1 :: Tree Int
tree1 = NodeT 1 (NodeT 1 (NodeT 2 EmptyT EmptyT) EmptyT) EmptyT

tree2 :: Tree Int
tree2 = NodeT 1
        (NodeT 1 (NodeT 2 EmptyT EmptyT)
        (NodeT 2 EmptyT EmptyT)) EmptyT

tree3 :: Tree Int
tree3 = NodeT 10
        (NodeT 20 (NodeT 40 EmptyT EmptyT) (NodeT 50 EmptyT EmptyT))
        (NodeT 30 (NodeT 40 EmptyT EmptyT) (NodeT 50 EmptyT EmptyT))

tree4 :: Tree Int
tree4 = NodeT 1
          (NodeT 2 (NodeT 4 EmptyT EmptyT) (NodeT 5 EmptyT EmptyT))
          (NodeT 3 EmptyT EmptyT)

tree5 :: Tree Char
tree5 = NodeT 'a'
          (NodeT 'b'
           (NodeT 'd' EmptyT EmptyT) EmptyT)
          (NodeT 'c'
           (NodeT 'e'
            EmptyT
            (NodeT 'g' EmptyT EmptyT))
           (NodeT 'f'
            (NodeT 'h' EmptyT EmptyT)
            (NodeT 'i' EmptyT EmptyT)))

tree6 :: Tree Int
tree6 = NodeT 25
          (NodeT 15
            (NodeT 10
              (NodeT 4 EmptyT EmptyT)
              (NodeT 12 EmptyT EmptyT))
            (NodeT 22
              (NodeT 18 EmptyT EmptyT)
              (NodeT 24 EmptyT EmptyT)))
          (NodeT 50
              (NodeT 35
                (NodeT 31 EmptyT EmptyT)
                (NodeT 44 EmptyT EmptyT))
              (NodeT 70
                (NodeT 66 EmptyT EmptyT)
                (NodeT 90 EmptyT EmptyT)))

-- Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT :: Tree Int -> Int
sumarT EmptyT          = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2

-- Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
-- en inglés).
sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT _ t1 t2) = 1 + sizeT t1 + sizeT t2

-- Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT :: Tree Int -> Tree Int
mapDobleT (NodeT n t1 t2) = NodeT (n * 2) (mapDobleT t1) (mapDobleT t2)
mapDobleT n               = n

-- Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
-- árbol.
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _  EmptyT           = False
perteneceT e1 (NodeT e2 t1 t2) = e1 == e2 || perteneceT e1 t1 || perteneceT e1 t2

-- Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
-- iguales a e.
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _  EmptyT           = 0
aparicionesT e1 (NodeT e2 t1 t2) = unoSi (e1 == e2) + aparicionesT e1 t1 + aparicionesT e1 t2

-- Dado un árbol devuelve los elementos que se encuentran en sus hojas.
leaves :: Tree a -> [a]
leaves EmptyT                  = []
leaves (NodeT a EmptyT EmptyT) = [a]
leaves (NodeT _ t1     t2)     = leaves t1 ++ leaves t2

-- Dado un árbol devuelve su altura.
-- Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad
-- de niveles del árbol1. La altura para EmptyT es 0, y para una hoja es 1.
heightT :: Tree a -> Int
heightT EmptyT                  = 0
heightT (NodeT _ EmptyT EmptyT) = 1
heightT (NodeT _ t1     t2)     = 1 + max (heightT t1) (heightT t2)

-- Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho,
-- en cada nodo del árbol.
mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT a t1 t2) = NodeT a (mirrorT t2) (mirrorT t1)

-- Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
-- Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo, luego la raiz
-- y luego los elementos del hijo derecho.
toList :: Tree a -> [a]
toList EmptyT                  = []
toList (NodeT a EmptyT EmptyT) = [a]
toList (NodeT a t1     t2)     = toList t1 ++ [a] ++ toList t2

-- Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un
-- nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
-- distancia de la raiz a uno de sus hijos es 1.
-- Nota: El primer nivel de un árbol (su raíz) es 0.
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT          = []
levelN 0 (NodeT a _ _)   = [a]
levelN n (NodeT _ t1 t2) = levelN (n - 1) t1 ++ levelN (n - 1) t2


-- Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de
-- dicho árbol.
hastaNivel :: Int -> Tree a -> [[a]]
hastaNivel 0 t = [levelN 0 t]
hastaNivel n t = levelN n t : hastaNivel (n - 1) t

listPerLevel :: Tree a -> [[a]]
listPerLevel t = hastaNivel (heightT t) t

-- Devuelve los elementos de la rama más larga del árbol
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT                  = []
ramaMasLarga (NodeT a EmptyT EmptyT) = [a]
ramaMasLarga (NodeT a t1 t2)         = a : ramaMasLarga (if heightT t1 > heightT t2 then t1 else t2)

-- Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta las hojas.

consAll :: a -> [[a]] -> [[a]]
consAll x []       = []
consAll x (xs:xss) = (x:xs) : consAll x xss

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT                  = []
todosLosCaminos (NodeT x EmptyT EmptyT) = [[x]]
todosLosCaminos (NodeT x t1 t2)         = consAll x (todosLosCaminos t1 ++ todosLosCaminos t2)

-----------------------------

data ExpA = Valor Int
          | Sum ExpA ExpA
          | Prod ExpA ExpA
          | Neg ExpA
          deriving Show

exp0 :: ExpA
exp0 = Sum (Valor 1) (Valor 2)

exp1 = Prod exp0 (Valor 3)
exp2 = Neg exp1
exp3 = Sum (Valor 0) (Valor 3)

-- Dada una expresión aritmética devuelve el resultado evaluarla.
eval :: ExpA -> Int
eval (Valor a)         = a
eval (Sum   exp1 exp2) = eval exp1 + eval exp2
eval (Prod  exp1 exp2) = eval exp1 * eval exp2
eval (Neg   exp)       = negate (eval exp)

-- Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando
-- notación matemática convencional):
-- a) 0 + x = x + 0 = x
-- b) 0 * x = x * 0 = 0
-- c) 1 * x = x * 1 = x
-- d ) - (- x) = x

-- x = 4
a = Sum (Valor 0) (Valor 4)
a' = Sum (Valor 4) (Valor 0)
b = Prod (Valor 0) (Valor 4)
b' = Prod (Valor 4) (Valor 0)
c = Prod (Valor 1) (Valor 4)
c' = Prod (Valor 4) (Valor 1)
d = Neg (Neg (Valor 4))

esCero :: ExpA -> Bool
esCero (Valor 0) = True
esCero _         = False

simplificarSum :: ExpA -> ExpA -> ExpA
simplificarSum (Valor 0) exp2      = exp2
simplificarSum exp1      (Valor 0) = exp1
simplificarSum exp1      exp2      = Sum exp1 exp2

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Valor 0) _ = Valor 0
simplificarProd _         (Valor 0) = Valor 0
simplificarProd (Valor 1) a = a
simplificarProd a         (Valor 1) = a
simplificarProd exp1 exp2 = Prod exp1 exp2

simplificarNeg :: ExpA -> ExpA
simplificarNeg (Neg exp) = exp
simplificarNeg exp = Neg exp

simplificar :: ExpA -> ExpA
simplificar (Sum exp1 exp2) = simplificarSum exp1 exp2
simplificar (Prod exp1 exp2) = simplificarProd exp1 exp2
simplificar (Neg exp) = simplificarNeg exp
simplificar exp = exp
