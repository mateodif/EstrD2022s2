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

--------------------------------------------------------------------

data Dir = Izq | Der
data Objeto = Tesoro | Chatarra
data Cofre = Cofre [Objeto]
data Mapa = Fin Cofre
          | Bifurcacion Cofre Mapa Mapa
          
-- Indica si hay un tesoro en alguna parte del mapa.
-- esTesoro :: Cofre -> Bool
-- esTesoro (Cofre objs) =

-- hayTesoro :: Mapa -> Bool
-- hayTesoro (Fin c) = esTesoro c

-- Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
-- lista vacía de direcciones.
-- hayTesoroEn :: [Dir] -> Mapa -> Bool

-- Indica el camino al tesoro. Precondición: existe un tesoro y es único.
-- caminoAlTesoro :: Mapa -> [Dir]

-- Indica el camino de la rama más larga.
-- caminoDeLaRamaMasLarga :: Mapa -> [Dir]

-- Devuelve los tesoros separados por nivel en el árbol.
-- tesorosPorNivel :: Mapa -> [[Objeto]]

-- Devuelve todos lo caminos en el mapa.
-- todosLosCaminos :: Mapa -> [[Dir]]


type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
          | Explorador Nombre [Territorio] Lobo Lobo
          | Cria Nombre
data Manada = M Lobo

p :: Presa
p = "rata"

t :: Territorio
t = "Asia"

c1, c2, c3 :: Lobo
c1 = Cria "a"
c2 = Cria "b"
c3 = Cria "c"

cc1, cc2 :: Lobo
cc1 = Cazador "Feo" [p] c1 c2 c3
cc2 = Explorador "Fea" [t] c1 c2

e1 :: Lobo
e1 = Explorador "Mijo" [t] cc1 cc2

m :: Manada
m = M e1

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M l) = exploradoresPorTerritorioLobo l

exploradoresPorTerritorioLobo :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioLobo (Cria n) = []
exploradoresPorTerritorioLobo (Explorador n ts l1 l2) =
  combinarEpt (crearListaDePares ts [n])
              (combinarEpt (exploradoresPorTerritorioLobo l1)
                           (exploradoresPorTerritorioLobo l2))
exploradoresPorTerritorioLobo (Cazador n ps l1 l2 l3) =
  combinarEpt (exploradoresPorTerritorioLobo l1)
              (combinarEpt (exploradoresPorTerritorioLobo l2)
                           (exploradoresPorTerritorioLobo l3))

combinarEpt :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
combinarEpt []     ys = ys
combinarEpt (x:xs) ys = actualizarTerritoriosConNombres x (combinarEpt xs ys)

actualizarTerritoriosConNombres :: (Territorio, [Nombre]) -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
actualizarTerritoriosConNombres (t,ns) []            = [(t,ns)]
actualizarTerritoriosConNombres (t,ns) ((t',ns'):xs) =
  if t == t'
    then (t,ns ++ ns') : xs
    else (t',ns') : actualizarTerritoriosConNombres (t,ns) xs

crearListaDePares :: [a] -> b -> [(a,b)]
crearListaDePares []     _ = []
crearListaDePares (x:xs) y = (x,y) : crearListaDePares xs y
