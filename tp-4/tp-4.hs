data Pizza = Prepizza
           | Capa Ingrediente Pizza
data Ingrediente = Salsa
                 | Queso
                 | Jamon
                 | Aceitunas Int

-- Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p

-- Dada una lista de ingredientes construye una pizza
armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (i:is) = Capa i (armarPizza is)

-- Le saca los ingredientes que sean jamón a la pizza
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa Jamon p) = sacarJamon p
sacarJamon (Capa i p) = Capa i (sacarJamon p)

-- Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En
-- particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.)
esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas _) = True
esAceituna _             = False

esMismoIngrediente :: Ingrediente -> Ingrediente -> Bool
esMismoIngrediente Salsa         Salsa         = True
esMismoIngrediente Queso         Queso         = True
esMismoIngrediente Jamon         Jamon         = True
esMismoIngrediente (Aceitunas _) (Aceitunas _) = False
esMismoIngrediente _             _             = False

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso i =
  esMismoIngrediente i Salsa || esMismoIngrediente i Queso

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa i p) = esSalsaOQueso i && tieneSoloSalsaYQueso p

-- Recorre cada ingrediente y si es aceitunas duplica su cantidad
dupCantAceitunas :: Ingrediente -> Ingrediente
dupCantAceitunas (Aceitunas i) = Aceitunas (i * 2)

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa i p) =
  let ing = if esAceituna i then dupCantAceitunas i else i
   in Capa ing (duplicarAceitunas p)

-- Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
-- ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasYPizza :: Pizza -> (Int, Pizza)
cantCapasYPizza p = (cantidadDeCapas p, p)

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza []     = []
cantCapasPorPizza (p:ps) = cantCapasYPizza p : cantCapasPorPizza ps





-- data Lobo = Cr Nombre
--           | Ex Nombre [Territorio] Lobo Lobo
--           | Ca Nombre [Presa] Lobo Lobo Lobo

-- ept :: Manada -> [(Territorio, [Nombre])]

-- eptL :: Lobo -> [(Territorio, [Nombre])]
-- eptL (Cr n) = []
-- eptL (Ex n ts l1 l2) = combinarEpt (mkListaDePares ts [n]) (combinarEpt (eptL l1) (eptL l2))
-- eptL (Ca n ps l1 l2 l3) = combinarEpt (eptL l1) (combinarEpt (eptL l2) (eptL l3))

-- combinarEpt :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
-- combinarEpt []     ys = ys
-- combinarEpt (x:xs) ys = agregarNs x (combinarEpt ys)

-- agregarNs :: (Territorio, [Nombre]) -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
-- agregarNs (t,ns) [] = [(t,ns)]
-- agregarNs (t,ns) ((t',ns'):xs) = if t == t'
--                                     then (t, ns ++ ns') : xs
--                                     else (t',ns') : agregarNs (t,ns) xs

-- mkListaDePares :: [a] -> b -> [(a,b)]
