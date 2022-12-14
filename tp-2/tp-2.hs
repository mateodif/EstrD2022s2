-- Funciones del tp 1
esSuperior :: TipoDePokemon -> TipoDePokemon -> Bool
esSuperior Agua   Fuego  = True
esSuperior Fuego  Planta = True
esSuperior Planta Agua   = True
esSuperior _      _      = False

esMismoTipoDePokemon :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipoDePokemon Fuego  Fuego  = True
esMismoTipoDePokemon Agua   Agua   = True
esMismoTipoDePokemon Planta Planta = True
esMismoTipoDePokemon _      _      = False

-- Fin funciones del tp 1

unoSi :: Bool -> Int
unoSi b = if b then 1 else 0

-- Dada una lista de enteros devuelve la suma de todos sus elementos.
sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (x:xs) = x + sumatoria xs

-- Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
-- de elementos que posee.
sumarUno :: a -> Int -> Int
sumarUno x y = y + 1

longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

-- Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (x:xs) = (x + 1) : (sucesores xs)

-- Dada una lista de booleanos devuelve True si todos sus elementos son True.
-- Precondición: la lista no debe ser vacía
conjuncion :: [Bool] -> Bool
conjuncion []     = True
conjuncion (x:xs) = x && conjuncion xs

-- Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion :: [Bool] -> Bool
disyuncion []     = False
disyuncion (x:xs) = x || disyuncion xs

-- Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar :: [[a]] -> [a]
aplanar []     = []
aplanar (x:xs) = x ++ aplanar xs

-- Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual
-- a e.
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []     = False
pertenece e (x:xs) = e == x || pertenece e xs

-- Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones :: Eq a => a -> [a] -> Int
apariciones _ []     = 0
apariciones e (x:xs) = (if e == x then 1 else 0) + apariciones e xs

-- Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ []     = []
losMenoresA n (x:xs) =
  if n > x
    then x : losMenoresA n xs
    else losMenoresA n xs

-- Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
-- de n elementos.
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ []     = []
lasDeLongitudMayorA n (x:xs) =
  if longitud x > n
    then x : lasDeLongitudMayorA n xs
    else lasDeLongitudMayorA n xs

-- Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la
-- lista
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal []     a = [a]
agregarAlFinal (x:xs) a = x : agregarAlFinal xs a

-- Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
-- elementos de la segunda a continuación. Definida en Haskell como (++).
agregar :: [a] -> [a] -> [a]
agregar []     l  = l
agregar (x:xs) l  = x : agregar xs l

-- Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida
-- en Haskell como reverse.
reversa :: [a] -> [a]
reversa []     = []
reversa (x:xs) = agregar (reversa xs) [x]

-- Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
-- máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
-- las listas no necesariamente tienen la misma longitud.
maximoEntreDos :: Int -> Int -> Int
maximoEntreDos a b = if a > b then a else b

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos []       []     = []
zipMaximos []       a      = a
zipMaximos a        []     = a
zipMaximos (x:xs)   (y:ys) = agregar [maximoEntreDos x y] (zipMaximos xs ys)

-- Dada una lista devuelve el mínimo
-- Precondicion: debe contener al menos un elemento
elMinimo :: Ord a => [a] -> a
elMinimo [x]    = x
elMinimo (x:xs) = min x (elMinimo xs)

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
losPrimeros _ []     = []
losPrimeros n (x:xs) = if n == 0 then [] else x : losPrimeros (n - 1) xs

-- Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
-- recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros _ []     = []
sinLosPrimeros 0 a      = a
sinLosPrimeros n (x:xs) = if n == 1 then xs else sinLosPrimeros (n - 1) xs

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
sumaDeEdadesDePersonas [] = 0
sumaDeEdadesDePersonas (x : xs) = edadDePersona x + sumaDeEdadesDePersonas xs

promedioEdad :: [Persona] -> Int
promedioEdad l = div (sumaDeEdadesDePersonas l) (longitud l)

-- Dada una lista de personas devuelve la persona más vieja de la lista.
-- Precondición: la lista al menos posee una persona
elMaximo :: Ord a => [a] -> a
elMaximo [x]    = x
elMaximo (x:xs) = max x (elMaximo xs)

edadesDePersonas :: [Persona] -> [Int]
edadesDePersonas [] = []
edadesDePersonas (x:xs) = edadDePersona x : edadesDePersonas xs

obtenerPersonaSegunEdad :: Int -> [Persona] -> Persona
obtenerPersonaSegunEdad _ [] = error "No existe una persona con la edad dada"
obtenerPersonaSegunEdad n (x:xs) = if edadDePersona x == n then x else obtenerPersonaSegunEdad n xs

elMasViejo :: [Persona] -> Persona
elMasViejo [] = error "Se necesita al menos una Persona"
elMasViejo [p]    = p
elMasViejo (p:ps) =
  if edadDePersona p > edadDePersona (elMasViejo ps)
     then p
     else elMasViejo ps

-------------------------

data TipoDePokemon = Agua | Fuego | Planta

data Pokemon = ConsPokemon TipoDePokemon Int

data Entrenador = ConsEntrenador String [Pokemon]

-- Devuelve la cantidad de Pokémon que posee el entrenador.
cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ ps) = longitud ps

-- Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
tipoDePokemon :: Pokemon -> TipoDePokemon
tipoDePokemon (ConsPokemon t _) = t

tiposDePokemones :: [Pokemon] -> [TipoDePokemon]
tiposDePokemones []     = []
tiposDePokemones (p:ps) = tipoDePokemon p : (tiposDePokemones ps)

contarPokemonesDeTipo :: TipoDePokemon -> [Pokemon] -> Int
contarPokemonesDeTipo t []     = 0
contarPokemonesDeTipo t (p:ps) =
  (if esMismoTipoDePokemon t (tipoDePokemon p) then 1 else 0) + contarPokemonesDeTipo t ps

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t (ConsEntrenador _ ps) = contarPokemonesDeTipo t ps

-- Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían
-- a los Pokemon del segundo entrenador.
esSuperiorAAlgunPokemon :: Pokemon -> [Pokemon] -> Bool
esSuperiorAAlgunPokemon p [] = False
esSuperiorAAlgunPokemon p (pp:pps) =
  esSuperior (tipoDePokemon p) (tipoDePokemon pp) || esSuperiorAAlgunPokemon p pps

contarPokemonesQueGanan :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
contarPokemonesQueGanan _ []     _   = 0
contarPokemonesQueGanan t (p:ps) pps =
  (if esMismoTipoDePokemon t (tipoDePokemon p) && esSuperiorAAlgunPokemon p pps
   then 1
   else 0) + contarPokemonesQueGanan t ps pps

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ t (ConsEntrenador _ ps1) (ConsEntrenador _ ps2) =
  contarPokemonesQueGanan t ps1 ps2

-- Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.

hayTipoDePokemonEnLista :: TipoDePokemon -> [Pokemon] -> Bool
hayTipoDePokemonEnLista _ []     = False
hayTipoDePokemonEnLista t (p:ps) = esMismoTipoDePokemon t (tipoDePokemon p) || hayTipoDePokemonEnLista t ps

hayCadaTipoPokemon :: [Pokemon] -> Bool
hayCadaTipoPokemon ps =
  hayTipoDePokemonEnLista Agua ps &&
  hayTipoDePokemonEnLista Planta ps &&
  hayTipoDePokemonEnLista Fuego ps

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ ps) = hayCadaTipoPokemon ps

--------------------------------------------------------

data Seniority = Junior | SemiSenior | Senior
  deriving Show

data Proyecto = ConsProyecto String
  deriving Show

data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
  deriving Show

data Empresa = ConsEmpresa [Rol]
  deriving Show

senirity0 :: Seniority
senirity0 = Senior

proyecto0 :: Proyecto
proyecto0 = ConsProyecto "Biblioteca"

rol0 :: Rol
rol0 = Developer senirity0 proyecto0

empresa0 :: Empresa
empresa0 = ConsEmpresa [rol0]

-- Definir las siguientes funciones sobre el tipo Empresa:

proyectoDeRol :: Rol -> Proyecto
proyectoDeRol (Developer _ p) = p

rolesDeEmpresa :: Empresa -> [Rol]
rolesDeEmpresa (ConsEmpresa roles) = roles

proyectosDeCadaRol :: [Rol] -> [Proyecto]
proyectosDeCadaRol [] = []
proyectosDeCadaRol (r:rs) = proyectoDeRol r : proyectosDeCadaRol rs

-- Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos :: Empresa -> [Proyecto]
proyectos e = proyectosDeCadaRol (rolesDeEmpresa e)

nombreDeProyecto :: Proyecto -> String
nombreDeProyecto (ConsProyecto s) = s

esMismoProyecto :: Proyecto -> Proyecto -> Bool
esMismoProyecto p1 p2 = nombreDeProyecto p1 == nombreDeProyecto p2

perteneceAAlgunProyecto :: Rol -> [Proyecto] -> Bool
perteneceAAlgunProyecto _ []     = False
perteneceAAlgunProyecto r (p:ps) = esMismoProyecto (proyectoDeRol r) p || perteneceAAlgunProyecto r ps

esMismoSeniority :: Seniority -> Seniority -> Bool
esMismoSeniority Junior     Junior     = True
esMismoSeniority SemiSenior SemiSenior = True
esMismoSeniority Senior     Senior     = True
esMismoSeniority _          _          = False

esSenior :: Rol -> Bool
esSenior (Developer s _) = esMismoSeniority s Senior

construirEmpresa :: [Rol] -> Empresa
construirEmpresa rs = ConsEmpresa rs

-- Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
-- además a los proyectos dados por parámetro.
rolesSeniorPertenecientesAProyectos :: [Rol] -> [Proyecto] -> Int
rolesSeniorPertenecientesAProyectos [] _ = 0
rolesSeniorPertenecientesAProyectos (r:rs) ps =
  (if esSenior r && perteneceAAlgunProyecto r ps then 1 else 0) + rolesSeniorPertenecientesAProyectos rs ps

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa rs) ps = rolesSeniorPertenecientesAProyectos rs ps

-- Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
rolesPertenecientesAProyectos :: [Rol] -> [Proyecto] -> Int
rolesPertenecientesAProyectos [] _ = 0
rolesPertenecientesAProyectos (r:rs) ps =
  (if perteneceAAlgunProyecto r ps then 1 else 0) + rolesPertenecientesAProyectos rs ps


cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn [] _                = 0
cantQueTrabajanEn ps (ConsEmpresa rs) = rolesPertenecientesAProyectos rs ps

-- Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
-- cantidad de personas involucradas.
asignadosSegunRoles :: [Proyecto] -> [Rol] -> [(Proyecto, Int)]
asignadosSegunRoles []     _ = []
asignadosSegunRoles (p:ps) rs = (p, rolesPertenecientesAProyectos rs [p]) : asignadosSegunRoles ps rs

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa rs) = asignadosSegunRoles (proyectosDeCadaRol rs) rs
