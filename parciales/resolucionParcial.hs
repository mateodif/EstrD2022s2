type Cliente = String

data Terminal = Boca1 | Boca2 deriving (Eq)

type Ruta = [Terminal]

data Dualnet = DN (Switch Cliente) (Map Cliente Ruta)

{- INV.REP: DN
    * Si un cliente es clave en el map, entonces está conectado en el switch con la ruta que tiene como valor asociado
    en el map.
    * Si un cliente está conectado en el switch entonces está como clave en el map y tiene asociada la ruta que permite
    llegar a dicha conexion.
    * Dado lo anterior no es posible que 2 valores del map sean iguales.

-}

--O(1)
--porque los constructos también son O(1)
emptyDN :: Dualnet
emptyDN = DN newSw emptyM

--O(1)
--porque sizeM es O(1)
cantidadDeClientesConectados :: Dualnet -> Int
cantidadDeClientesConectados (DN _ m) = sizeM m

--O(2^n), siendo r la longitud de la ruta dada
--por disponiblesADistancia
--costo real (error enunciado) O(r * 2^r)
estaDisponible :: Ruta -> Dualnet -> Bool
estaDisponible r (DN s _) = elem r $ disponiblesADistancia s (length r)

--O(L + log C)
--desconectar es O(r) y conectar también
--log C por lookupM sobre la cantidad total de clientes
--L porque el cliente podría tener asginada la ruta de mayor longitud de la estructura y desconectar es O(r) para L
conectarCliente :: Ruta -> Cliente -> Dualnet -> Dualnet
conectarCliente r c (DN s m) =
  case lookupM c m of
    Just r' -> DN (conectar r' c (desconectar r' s)) (assocM c r m)
    Nothing -> DN (conectar r c s) (assocM c r m)

--O(L + C log C)
--O(C) por keys m
-- + O(C * (log C + log C +  L))
-- => O(C) + O(C * (log C + L))
-- => O(C (log C + L))
-- => O((C log C) + (C * L))
pinPorCliente :: Dualnet -> Heap (Int, Cliente)
pinPorCliente (DN _ m) = pinPorCliente' (keys m) m

-- O(n * (log m + log n + length r))
--O(n) por recursion sobre longitud de la lista

-- * (log m por lookupM siendo m la cantidad de claves del map

--    + log n por los insert que corresponden con la longitud de la lista
--    + r) siendo r la longitud mas larga.
pinPorCliente' :: [Cliente] -> Map Cliente Ruta -> Heap (Int, Cliente)
pinPorCliente' [] _ = emptyH
pinPorCliente' (c : cs) m =
  let Just r = lookupM c m
   in insertH (length r, c) (pinPorCliente' cs m)

{-INV.REP:
    * No puede haber un conmutador sin algun dato conectado o bien en su red privada o en alguna de sus bocas.
        OBSERVACIÓN: no es válida la estructura Conmutador Disponible Terminal Terminal.

-}

newSw :: Switch a
newSw = Terminal

--O(r) siendo r la longitud de la ruta dada.
--se hace recursion sobre la lista y en cada instancia la
conectar :: Ruta -> a -> Switch a -> Switch a
conectar [] x Terminal = Conmutador (Conexion x) Terminal Terminal
conectar [] x (Conmutador rp s1 s2) = case rp of
  Conexion _ -> error "la conexion no está disponible"
  Disponible -> Conmutador (Conexion x) s1 s2
conectar _ x Terminal = error "la conexion no es alcanzable"
conectar (r : rs) x (Conmutador rp s1 s2) = case r of
  Boca1 -> Conmutador rp (conectar rs x s1) s2
  Boca2 -> Conmutador rp s1 (conectar rs x s2)

--O(r) siendo r la longitud de la ruta dada.
desconectar :: Ruta -> Switch a -> Switch a
desconectar _ Terminal = error "la ruta NO está siendo utilizada"
desconectar [] (Conmutador rp s1 s2) = case rp of
  Conexion _ -> disponible s1 s2
  Disponible -> error "la ruta NO está siendo utilizada"
desconectar (r : rs) (Conmutador rp s1 s2) = case r of
  Boca1 -> Conmutador rp (desconectar rs s1) s2
  Boca2 -> Conmutador rp s1 (desconectar rs s2)

disponible Terminal Terminal = Terminal
disponible s1 s2 = Conmutador Disponible s1 s2

disponiblesADistancia :: Switch a -> Int -> [Ruta]
disponiblesADistancia Terminal 0 = [[]]
disponiblesADistancia (Conmutador rp s1 s2) 0 = case rp of
  Conexion _ -> []
  Disponible -> [[]]
disponiblesADistancia Terminal n = []
disponiblesADistancia (Conmutador rp s1 s2) n =
  agregarle Boca1 (disponiblesADistancia s1 (n - 1))
    ++ agregarle Boca2 (disponiblesADistancia s2 (n - 1))

--O(n)
agregarle :: Terminal -> [Ruta] -> [Ruta]
agregarle _ [] = []
agregarle t (r : rs) = (x : xs) : agregarle t rs
