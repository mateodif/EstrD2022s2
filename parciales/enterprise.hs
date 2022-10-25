{-
La interfaz de Map, siendo N la cantidad de claves distintas en el map:
    emptyM :: Map k v O(1)
    assocM :: k -> v -> Map k v -> Map k v O(log N)
    lookupM :: Map k v -> k -> Maybe v O(log N)
    removeM :: Map k v -> k -> Map k v O(log N)
    domM :: Map k v -> [k] O(N)

La interfaz de Set, siendo N la cantidad de elementos del conjunto:
    emptyS :: Set a O(1)
    addS :: a -> Set a -> Set a O(log N)
    removeS :: a -> Set a -> Set a O(log N)
    belongs :: a -> Set a -> Bool O(log N)
    union :: Set a -> Set a -> Set a O(N log N)
    intersection :: Set a -> Set a -> Set a O(N log N)
    set2list :: Set a -> [a] O(N)
    sizeS :: Set a -> Int O(1)

La interfaz de Heap, siendo N la cantidad de elementos de la heap:
    emptyH :: Heap a O(1)
    isEmptyH :: Heap a -> Bool O(1)
    insertH :: a -> Heap a -> Heap a O(log N)
    findMin :: Heap a -> a O(1)
    deleteMin :: Heap a -> Heap a O(log N)
    splitMin :: Heap a -> (a, Heap a) O(log N)
-}


data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)
{-
Inv. Rep.
    a. Cada Tripulante solo puede aparecer en un determinado sector
    b. Todos los Tripulantes que aparecen en el map deben aparecer en el heap tambien y viceversa
    c. El "sector con mas tripulantes" debe existir en el mapa y viceversa
    d. No puede existir una nave sin sectores
-}

-- Propósito: Crea una nave con todos esos sectores sin tripulantes.
-- Precondición: la lista de sectores no está vacía
-- Costo: O(S log S) siendo S la cantidad de sectores de la lista.
-- Costo real: O(S log S) ya que crearMapDeSectores es O(n log n) y
-- estamos recorriendo todos los sectores de la nave
-- emptyH, head son O(1) y se desprecian
naveVacia :: [Sector] -> Nave
naveVacia ss = MkN (crearMapDeSectores ss) emptyH (head ss, 0)


-- O(n log n) donde n es la cantidad de sectores en la lista
-- en el peor caso, la recursion debe recorrer toda la lista, por lo que seria O(n)
-- y ademas, en cada recursion, usamos assocM que tiene un costo O(log n)
-- por lo tanto, O(n log n)
crearMapDeSectores :: [Sector] -> Map Sector (Set Tripulante)
crearMapDeSectores [] = emptyM
crearMapDeSectores (s:ss) = assocM s emptyS $ crearMapDeSectores ss

-- Propósito: Obtiene los tripulantes de un sector.
-- Costo: O(log S) siendo S la cantidad de sectores.
-- Costo real: O(log S) ya que lookupM es log N y
-- en el peor de los casos recorremos todo el map
tripulantesDe :: Sector -> Nave -> Set Tripulante
tripulantesDe s (MkN ss _ _) = fromJust $ lookupM ss s


-- Propósito: Denota los sectores de la nave
-- Costo: O(S) siendo S la cantidad de sectores.
-- Costo real: O(S) ya que domM es O(n) y estamos
-- recorriendo todas las keys del map de sectores
sectores :: Nave -> [Sector]
sectores (MkN ss _ _) = domM ss

-- Propósito: Denota el tripulante con mayor rango.
-- Precondición: la nave no está vacía.
-- Costo: O(1).
-- Costo real: O(1) porque findMin es O(1)
conMayorRango :: Nave -> Tripulante
conMayorRango (MkN _ h _) = findMin h

-- Propósito: Denota el sector de la nave con más tripulantes.
-- Costo: O(1).
-- Costo real: O(1) porque acceder al primer elemento de
-- una tupla es O(1)
conMasTripulantes :: Nave -> Sector
conMasTripulantes (MkN _ _ (s,_)) = s

-- Propósito: Denota el conjunto de tripulantes con dicho rango.
-- Costo: O(P log P) siendo P la cantidad de tripulantes.
conRango :: Rango -> Nave -> Set Tripulante
conRango r (MkN _ h _) = tripulantesPorRango r h

tripulantesPorRango :: Rango -> Heap Tripulante -> Set Tripulante
tripulantesPorRango r h =
  if isEmptyH h
    then emptyS
    else
      let (t, ts) = splitMin h
       in if rango t == r
          then addS t $ tripulantesPorRango ts
          else tripulantesPorRango ts


-- Propósito: Devuelve el sector en el que se encuentra un tripulante.
-- Precondición: el tripulante pertenece a la nave.
-- Costo: O(S log S log P) siendo S la cantidad de sectores y P la cantidad de tripulantes.
sectorDe :: Tripulante -> Nave -> Sector

-- Propósito: Agrega un tripulante a ese sector de la nave.
-- Precondición: El sector está en la nave y el tripulante no.
-- Costo: No hay datos (justifique su elección).
agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
