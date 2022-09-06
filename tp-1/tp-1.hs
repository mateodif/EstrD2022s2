sucesor :: Int -> Int
sucesor n = n + 1

--------

sumar :: Int -> Int -> Int
sumar a b = a + b

--------

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto a b = (div a b, mod a b)

--------

maxDelPar :: (Int, Int) -> Int
maxDelPar (a, b) = if a > b
                     then a
                     else b

-- Ejemplos
-- sumar (maxDePar (divisionYResto 16 2)) (sucesor 1)
-- sumar (sucesor 5) (maxDePar (divisionYResto 10 2))
-- sumar (sucesor (maxDePar (divisionYResto 8 2))) 5
-- sucesor (maxDePar (divisionYResto (sumar 17 1) 2))

--------

data Dir = Norte | Sur | Oeste | Este
     deriving Show

------------

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Oeste = Este
opuesto Este = Oeste

-------------

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Oeste Oeste = True
iguales Este Este = True
iguales _ _ = False

--------------

-- Precondición: no debe ser Oeste?
-- Es una función parcial porque posee una precondición
siguiente :: Dir -> Dir
siguiente Sur = Este
siguiente Este = Norte
siguiente Norte = Oeste

-------------

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving Show

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

-------------

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

---------------

numeroDeDia :: DiaDeSemana -> Int
numeroDeDia Lunes = 1
numeroDeDia Martes = 2
numeroDeDia Miercoles = 3
numeroDeDia Jueves = 4
numeroDeDia Viernes = 5
numeroDeDia Sabado = 6
numeroDeDia Domingo = 7

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues a b = numeroDeDia a > numeroDeDia b

--------------

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

---------------

negar :: Bool -> Bool
negar True = False
negar _ = True

----------------
-- Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
-- devuelve True. Esta función debe ser tal que implica False (error "Mal") devuelva
-- True.
-- Nota: no viene implementada en Haskell.

implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

----------------

yTambien :: Bool -> Bool -> Bool
yTambien True a = a
yTambien _ _ = False

-----------------

oBien :: Bool -> Bool -> Bool
oBien True a = True
oBien _ _ = False

-----------------

data Persona = P String Int
   deriving Show

nombre :: Persona -> String
nombre (P n _) = n

-----------------

edad :: Persona -> Int
edad (P _ e) = e

-----------------

crecer :: Persona -> Persona
crecer (P n e) = P n (sucesor e)

-----------------

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nombreNuevo (P _ e) = P nombreNuevo e

------------------

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

------------------

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2 then p1 else p2

-------------------

data TipoDePokemon = Agua | Fuego | Planta
   deriving Show

data Pokemon = Poke TipoDePokemon Int
   deriving Show

data Entrenador = E String Pokemon Pokemon
   deriving Show

esSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
esSuperior Agua Fuego = True
esSuperior Fuego Planta = True
esSuperior Planta Agua = True
esSuperior _ _ = False

superaA :: Pokemon -> Pokemon -> Bool
superaA (Poke t1 _) (Poke t2 _) = esSuperior t1 t2

----------------------

tipoDePokemon :: Pokemon -> TipoDePokemon
tipoDePokemon (Poke t _) = t

unoSiMismoPokemonCeroSino :: TipoDePokemon -> TipoDePokemon -> Int
unoSiMismoPokemonCeroSino Fuego Fuego = 1
unoSiMismoPokemonCeroSino Agua Agua = 1
unoSiMismoPokemonCeroSino Planta Planta = 1
unoSiMismoPokemonCeroSino _ _ = 0

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t (E _ p1 p2) =
  unoSiMismoPokemonCeroSino (tipoDePokemon p1) t + unoSiMismoPokemonCeroSino (tipoDePokemon p2) t

-----------------------

pokemonesDeEntrenador :: Entrenador -> [Pokemon]
pokemonesDeEntrenador (E _ p1 p2) = [p1, p2]

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (e1, e2) = pokemonesDeEntrenador e1 ++ pokemonesDeEntrenador e2

------------------------

loMismo :: a -> a
loMismo a = a

siempreSiete :: a -> Int
siempreSiete a = 7

swap :: (a,b) -> (b, a)
swap (a, b) = (b, a)

-- Por qué existen dos variables de tipo diferentes?
-- Porque debe funcionar para cualquier tupla formada por cualquier tipo

-- Responda la siguiente pregunta: ¾Por qué estas funciones son polimórficas?
-- Porque pueden recibir cualquier tipo de dato en sus argumentos

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

-----------------

elPrimero :: [a] -> a
elPrimero (x : _) = x

-----------------

sinElPrimero :: [a] -> [a]
sinElPrimero (_ : xs) = xs

-----------------

splitHead :: [a] -> (a, [a])
splitHead (x : xs) = (x, xs)
