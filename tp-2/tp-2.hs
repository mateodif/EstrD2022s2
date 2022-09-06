-- NOTE: No se si permiten hacer esto. Son mis helpers.
-- Casi todos los ejercicios se pueden resolver con versiones
-- genericas de foldr pero me parece que me estoy yendo de tema...

-- Voy a poner las soluciones normales en comentarios, asi lo puedo
-- revertir rapido por las dudas

-- INICIO HELPERS
compararSegunFuncion :: (a -> a -> Bool) -> [a] -> a
compararSegunFuncion _ [x] = x
compararSegunFuncion f (x : xs) =
  if f x (compararSegunFuncion f xs)
    then x
    else compararSegunFuncion f xs

-- esta funcion es analoga a "map"
miMap :: (a -> b) -> [a] -> [b]
miMap _ [] = []
miMap f (x : xs) = f x : miMap f xs

-- esta funcion es analoga a "reduce"
reducir :: (a -> b -> b) -> b -> [a] -> b
reducir f z [] = z
reducir f z (x : xs) = f x (reducir f z xs)

-- FIN HELPERS

-- Dada una lista de enteros devuelve la suma de todos sus elementos.
sumatoria :: [Int] -> Int
-- sumatoria []     = 0
-- sumatoria (x:xs) = x + sumatoria xs
sumatoria = reducir (+) 0

-- Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
-- de elementos que posee.
sumarUno :: a -> Int -> Int
sumarUno x y = y + 1

longitud :: [a] -> Int
-- longitud []     = 0
-- longitud (x:xs) = 1 + longitud xs
longitud = reducir sumarUno 0

-- Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores :: [Int] -> [Int]
-- sucesores []     = []
-- sucesores (x:xs) = (x + 1) : (sucesores xs)
sucesores = miMap sucesor

-- Dada una lista de booleanos devuelve True si todos sus elementos son True.
-- Precondición: la lista no debe ser vacía
conjuncion :: [Bool] -> Bool
-- conjuncion [] = True
-- conjuncion (x : xs) = x && conjuncion xs
conjuncion = reducir (&&) True

-- Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion :: [Bool] -> Bool
-- disyuncion [] = False
-- disyuncion (x : xs) = x || disyuncion xs
disyuncion = reducir (||) False

-- Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar :: [[a]] -> [a]
-- aplanar [] = []
-- aplanar (x : xs) = x ++ aplanar xs
aplanar = reducir (++) []

-- Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual
-- a e.
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x : xs) = e == x || pertenece e xs

-- Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones e (x : xs) = if e == x then 1 else 0 + apariciones e xs

-- Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ [] = []
losMenoresA n (x : xs) =
  if n > x
    then x : losMenoresA n xs
    else losMenoresA n xs

-- Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
-- de n elementos.
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA n (x : xs) =
  if longitud x > n
    then x : lasDeLongitudMayorA n xs
    else lasDeLongitudMayorA n xs

-- Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la
-- lista
agregarAlFinal :: [a] -> a -> [a]
-- agregarAlFinal [] a = [a]
-- agregarAlFinal (x : xs) a = x : agregarAlFinal xs a
agregarAlFinal xs a = reducir (:) [a] xs

-- Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
-- elementos de la segunda a continuación. Definida en Haskell como (++).
agregar :: [a] -> [a] -> [a]
agregar [] [] = []
agregar [] l = l
agregar (x : xs) l = x : agregar xs l

-- Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida
-- en Haskell como reverse.
reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = agregar (reversa xs) [x]

-- Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
-- máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
-- las listas no necesariamente tienen la misma longitud.

maximoEntreDos :: Int -> Int -> Int
maximoEntreDos a b = if a > b then a else b

zipCon :: (a -> b -> c) -> [a] -> [b] -> [c]
zipCon f (x : xs) (y : ys) = f x y : zipCon f xs ys
zipCon _ _ _ = []

zipMaximos :: [Int] -> [Int] -> [Int]
-- zipMaximos []       []     = []
-- zipMaximos []       _      = []
-- zipMaximos _        []     = []
-- zipMaximos (x:xs)   (y:ys) = agregar [maximoEntreDos x y] (zipMaximos xs ys)
zipMaximos = zipCon maximoEntreDos

-- Dada una lista devuelve el mínimo
-- Precondicion: debe contener al menos un elemento

-- elMinimo :: Ord a => [a] -> a
-- elMinimo [x]    = x
-- elMinimo (x:xs) = if x < elMinimo xs then x else elMinimo xs

elMinimo :: Ord a => [a] -> a
elMinimo = compararSegunFuncion (<)

-- Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
-- llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
-- n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n = if n < 1 then [] else n : cuentaRegresiva (n - 1)

-- Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces
-- Precondicion: no se puede repetir n negativas veces
repetir :: Int -> a -> [a]
repetir n e = if n == 0 then [] else e : repetir (n - 1) e

-- Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
-- Si la lista es vacía, devuelve una lista vacía.
losPrimeros :: Int -> [a] -> [a]
losPrimeros _ [] = []
losPrimeros n (x : xs) = if n == 0 then [] else x : losPrimeros (n - 1) xs

-- Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
-- recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros _ [] = []
sinLosPrimeros 0 a = a
sinLosPrimeros n (x : xs) = if n == 1 then xs else sinLosPrimeros (n - 1) xs

-- Definir el tipo de dato Persona, como un nombre y la edad de la persona.
data Persona = Persona
  { nombre :: String,
    edad :: Int
  }
  deriving (Show)

-- Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
edadDePersona :: Persona -> Int
edadDePersona (Persona _ e) = e

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA n (x : xs) = if edadDePersona x > n then x : mayoresA n xs else mayoresA n xs

-- Dada una lista de personas devuelve el promedio de edad entre esas personas. Precondición: la lista al menos posee una persona.
sumaDeEdadesDePersonas :: [Persona] -> Int
-- sumaDeEdadesDePersonas [] = 0
-- sumaDeEdadesDePersonas (x : xs) = edadDePersona x + sumaDeEdadesDePersonas xs
sumaDeEdadesDePersonas ps = reducir (+) 0 (miMap edadDePersona ps)


promedioEdad :: [Persona] -> Int
promedioEdad [] = 0
promedioEdad l = div (sumaDeEdadesDePersonas l) (longitud l)

-- Dada una lista de personas devuelve la persona más vieja de la lista.
-- Precondición: la lista al menos posee una persona
-- elMaximo :: Ord a => [a] -> a
-- elMaximo [x]    = x
-- elMaximo (x:xs) = if x > elMaximo xs then x else elMaximo xs

-- edadesDePersonas :: [Persona] -> [Int]
-- edadesDePersonas [] = []
-- edadesDePersonas (x:xs) = edadDePersona x : edadesDePersonas xs

-- obtenerPersonaSegunEdad :: Int -> [Persona] -> Persona
-- obtenerPersonaSegunEdad _ [] = error "No existe una persona con la edad dada"
-- obtenerPersonaSegunEdad n (x:xs) = if edadDePersona x == n then x else obtenerPersonaSegunEdad n xs

-- elMasViejo :: [Persona] -> Persona
-- elMasViejo l = obtenerPersonaSegunEdad (elMaximo (edadesDePersonas l)) l

esMayorQue :: Persona -> Persona -> Bool
esMayorQue p1 p2 = edadDePersona p1 > edadDePersona p2

elMasViejo :: [Persona] -> Persona
elMasViejo = compararSegunFuncion esMayorQue

data TipoDePokemon = Agua | Fuego | Planta

data Pokemon = ConsPokemon TipoDePokemon Int

data Entrenador = ConsEntrenador String [Pokemon]

-- Devuelve la cantidad de Pokémon que posee el entrenador.
pokemonesDeEntrenador :: Entrenador -> [Pokemon]
pokemonesDeEntrenador (ConsEntrenador _ ps) = ps

cantPokemon :: Entrenador -> Int
cantPokemon e = longitud (pokemonesDeEntrenador e)

-- TODO

-- Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
-- cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
-- cantPokemonDe t e = apariciones t (pokemonesDeEntrenador e) -- esto se puede hacer con miMap

-- Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían
-- a los Pokemon del segundo entrenador.
-- losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int

-- Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
-- esMaestroPokemon :: Entrenador -> Bool