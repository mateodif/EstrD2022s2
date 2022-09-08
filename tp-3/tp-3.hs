data Color = Azul | Rojo
  deriving Show

data Celda = Bolita Color Celda | CeldaVacia
  deriving Show

celda0 = CeldaVacia
celda1 = Bolita Rojo CeldaVacia
celda2 = Bolita Rojo (Bolita Azul CeldaVacia)
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
sumatoriaSegunColor c (x:xs) = unoSi (esMismoColor c x) + sumatoriaSegunColor c xs

-- Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
-- existe una operación sobre listas que ayude a resolver el problema.
nroBolitas :: Color -> Celda -> Int
nroBolitas co ce = sumatoriaSegunColor co (coloresDeBolitasEnCelda ce)

-- Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner co ce = Bolita co ce

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
ponerN 0 _ ce = ce
ponerN n co ce = ponerN (n - 1) co (poner co ce)

------------------------------------------------------

data Objeto = Cacharro | Tesoro
  deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
  deriving Show

tesoro0 = Fin
tesoro1 = Nada (Nada (Fin))
tesoro2 = Nada (Nada (Nada (Cofre [Cacharro] (Fin))))
tesoro3 = Nada (Nada (Nada (Cofre [Tesoro] (Nada (Fin)))))

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
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro (Cofre objs c) = if hayTesoroEnCofre objs then 0 else 1
pasosHastaTesoro (Nada c)       = 1 + pasosHastaTesoro c

-- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
-- pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n c = n == pasosHastaTesoro c

-- Indica si hay al menos “n” tesoros en el camino.
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n c = pasosHastaTesoro c > n

subCamino :: Int -> Camino -> Camino
subCamino 0 c           = c
subCamino n (Nada c)    = subCamino (n - 1) c
subCamino n (Cofre _ c) = subCamino (n - 1) c
subCamino n Fin         = subCamino (n - 1) Fin

-- Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
-- el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
-- incluidos tanto 3 como 5 en el resultado.
-- cantTesorosEntre :: Int -> Int -> Camino -> Int
