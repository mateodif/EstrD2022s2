module Nave where

data Nave
  = N
      (Map SectorId Sector)
      (Map Nombre Tripulante)
      (PriorityQueue Tripulante)
  deriving (Show)

{-
  Inv. Rep
  - si un tripulante existe como vsalor en el segundo map entonces
  existe en la PQ con el mismo rango y los mismos sectores
  - para cualquier tripulante que este registrado en el map debe existir
  como clave en el primer map cada uno de sus sectoresId y en el valor asociado
  a dicha clave debe haber un sector que contiene al tripulante anterior
  - cada sector registrado como valor en el primer map debe tener asociado
  en el segundo por cada uno de sus tripulante
  Observaciones

-}

-- O(S log S)
construir :: [SectorId] -> Nave
construir ss = N (armarS ss) emptyM emptyPQ

armarS :: [SectorId] -> (Map SectorId Sector)
armarS [] = emptyM
armarS (s : ss) = assocM s (crearS s) (armarS ss)

-- O(log T)
-- Observacion: si hay un tripulante con el nombre provisto, se omite
ingresarT :: Nombre -> Rango -> Nave -> Nave
ingresarT n r (N sm tm pq) =
  case lookupM n tm of
    Just _ -> N sm tm pq
    Nothing ->
      let t = crearT n r
       in N sm (assocM n t tm) (insertPQ t pq)

sectoresAsignados :: Nombre -> Nave -> Set SectorId
sectoresAsignados n (N sm tm pq) =
  case lookupM n tm of
    Just x -> sectoresT x
    Nothing -> error ("no existe el tripulante " ++ n)

datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
datosDeSector s (N sm tm pq) =
  case lookupM s sm of
    Just x -> (tripulantesS x, componentesS x)
    Nothing -> error ("no existe el sector " ++ s)

tripulantesN :: Nave -> [Tripulante]
tripulantesN (N sm tm pq) = pqToList pq

-- O(N log N)
-- porque se recorre todos los elementos
-- y en cada instancia de la recursion se utilizan
-- funciones de costo log sobre la estructura
pqToList :: Ord a => PQ a -> [a]
pqToList pq =
  if isEmptyPQ pq
    then []
    else findMaxPQ pq : pqToList (deleteMaxPQ pq)

-- si el sector no existe se crea sin tripulantes
-- O(C + log S), siendo C la cant de componentes dados
-- O(log S) por buscarOCrear sobre todos los sectores
-- + O(n) donde n es el tamaño de la lista provista
-- + log S por assocM
-- => O(n + log S)
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs sid (N sm tm pq) =
  let sector = buscarOCrear sid sm
   in N (assocM sid (agregarC sector cs) sm) tm pq

-- O(c)
-- por la recursion sobre los componentes
-- y por ser a agregarC constante
agregarCs [] sector = sector
agregarCs (x : xs) sector = agregarC x (agregarCs xs sector)

-- O(log n)
-- donde n es la cantidad de claves de sm
buscarOCrear sid sm =
  case lookupM sid sm of
    Just x -> x
    Nothing -> crearS sid

-- O(log S + log T + T log T)
asignarASector :: Nombre -> SectorId -> Nave -> Nave
asignarASector n sid (N sm tm pq) =
  let (Just s) = lookupM sid sm
      (Just t) = lookupM n tm
      s' = agregarT n s
      t' = asignarS sid t
   in N (assocM sid s' sm) (assocM n t' tm) (reemplazarPQ t' pq)

reemplazarPQ x pq =
  if isEmptyPQ pq
    then error ("el elemento " ++ show x ++ " no existe")
    else
      if x == findMaxPQ pq
        then insertPQ x (deleteMaxPQ pq)
        else insertPQ (findMaxPQ pq) (reemplazarPQ x (deleteMaxPQ pq))
