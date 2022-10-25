--------------
-- INTERFAZ -- 
--------------

data Organizador = 
    MkO (Map Checksum (Set Persona)) 
        (Map Persona  (Set Checksum))

{- 
    Inv.Rep. 
        a. Si un Checksum está asociado a un Set Persona, todas las Personas del set a su vez tienen ese Checksum asociado a su set Checksum 
        en el mp2. Y viceversa.
-}

-- Propósito: Un organizador vacío.
-- Eficiencia: O(1)
nuevo :: Organizador
nuevo = MkO emptyM emptyM 


-- Propósito: Agrega al organizador un programa con el Checksum indicado; el conjunto es el conjunto de personas autores
-- de dicho programa.
-- Precondición: el identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas
-- no está vacío.
-- Eficiencia: no hay ninguna garantía de eficiencia.
-- O(n log n + log n) = O(N log N)
agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
agregarPrograma (MkO mc mp) c sp = MkO (assocM c sp mc) (agregarPersonasYChecksum c sp)


-- lookupM: log N + addS: log N = 2log N
setConChecksum :: Map Persona (Set Checksum) -> Persona -> (Set Checksum) -> Checksum -> Set Checksum
setConChecksum mp p sc c = 
    case lookupM p mp of -- log N
        Just sc -> (addS sc c) -- log N
        Nothing -> (addS emptyS c) 


-- O(
    -- recursion de lista : p 
    -- log P : assocM 
    -- 2log P : setConChecksum
    -- p + 3log P = n log n
-- )
-- donde p es cada persona en la lista de personas
-- donde P es cada par persona:set en el mapa
agregarPersonasYChecksum :: Map Persona (Set Checksum) -> Checksum -> [Persona] -> Map Persona (Set Checksum)
agregarPersonasYChecksum mp _ [] = mp
agregarPersonasYChecksum mp c (p:ps) = assocM p (setConChecksum mp p sc c) (agregarPersonasYChecksum mp c ps)


-- Propósito: denota una lista con todos y cada uno de los códigos identificadores de programas del organizador.
-- Eficiencia: O(C) en peor caso, donde C es la cantidad de códigos en el organizador.
-- domM es O(M)
todosLosProgramas :: Organizador -> [Checksum]
todosLosProgramas (MkO mc _) = domM mc


-- Propósito: denota el conjunto de autores que aparecen en un programa determinado.
-- Precondición: el Checksum debe corresponder a un programa del organizador.
-- Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.
autoresDe :: Organizador -> Checksum -> Set Persona
autoresDe (MkO mc _) c = fromJust $ lookupM mc c


-- Propósito: denota el conjunto de programas en los que participó una determinada persona.
-- Precondición: la persona debe existir en el organizador.
-- Eficiencia: O(log P) en peor caso, donde P es la cantidad total de personas del organizador.
programasDe :: Organizador -> Persona -> Set Checksum
programasDe (MkO _ mp) p = fromJust $ lookupM mp p


-- Propósito: dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
-- Precondición: las personas deben ser distintas.
-- Eficiencia: O(log P + C log C) en peor caso, donde P es la cantidad de personas distintas que aparecen en todos los
-- programas del organizador, y C la cantidad total de programas.
-- programasDe: log P 
-- intersection: O(C log C)
-- isEmptyS, not: O(1)
-- => O(log P + C log C)
programaronJuntas :: Organizador -> Persona -> Persona -> Bool
programaronJuntas org p1 p2 = not (isEmptyS (intersection (programasDe org p1) (programasDe org p2)))


-- Propósito: dado un organizador y una persona, denota la cantidad de programas distintos en los que aparece.
-- Eficiencia: O(log P) en peor caso, donde P es la cantidad de personas del organizador.
nroProgramasDePersona :: Organizador -> Persona -> Int
nroProgramasDePersona (MkO _ mp) p = 
    case lookupM p mp of 
        Nothing -> 0 
        Just s -> sizeS s 

-------------
-- USUARIO --
-------------

-- Propósito: dadas dos personas y un organizador, denota el conjunto de aquellos programas en las que las personas
-- programaron juntas.
-- intersection: O(N log N)
-- programasDe: O(log P) -> P de programas de la persona
-- O(N log N + log N) = O(N log N)
programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
programasEnComun p1 p2 org = intersection (programasDe org p1) (programasDe org p2)


-- Propósito: denota verdadero si la persona indicada aparece como autor de todos los programas del organizador.
-- mroProgramasDePersona: O(log P) 
-- length: O(N)
-- todosLosProgramas: O(N)
-- O(P log P) -> donde P es la cantidad de programas de la persona
esUnGranHacker :: Organizador -> Persona -> Bool
esUnGranHacker org p = (nroProgramasDePersona org p) == length (todosLosProgramas org)


------------------------------
-- ORGANIZADOR CON CHECKSUM --
------------------------------

data Maybe a = Nothing | Just

data Organizador = 
    MkO (Map Checksum (Set Persona)) 
        (Map Persona  (Set Checksum))
        (Maybe Checksum)

{- 
    Inv.Rep. 
        a. Si un Checksum está asociado a un Set Persona, todas las Personas del set a su vez tienen ese Checksum asociado a su set Checksum 
        en el mp2. Y viceversa.
        b. El checksum debe existir en el map de checksums y en el set de checksums :)))
        
    Observacion: El Checksum debe pertenecer al programa con mas autores
-}


-- Propósito: Un organizador vacío.
-- Eficiencia: O(1)
nuevo :: Organizador
nuevo = MkO emptyM emptyM Nothing


-- O(log C) -> donde C es la cantidad de checksums en el mapa
checksumConMasAutores :: Map Checksum (Set Persona) -> Set Persona -> Maybe Checksum -> Checksum -> Maybe Checksum
checksumConMasAutores mp s1 c1 c2 = 
    case c1 of 
        Nothing -> Just c2 
        Just c1' -> case lookupM mp c1' of 
            Nothing -> error "esto no puede pasar"
            Just s2 -> if sizeS s1 > sizeS s2 then Just c1' else Just c2


-- Propósito: Agrega al organizador un programa con el Checksum indicado; el conjunto es el conjunto de personas autores
-- de dicho programa.
-- Precondición: el identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas
-- no está vacío.
-- Eficiencia: no hay ninguna garantía de eficiencia.
-- O(n log n + log n) = O(N log N)
agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
agregarPrograma (MkO mc mp c1) c2 sp = MkO (assocM c2 sp mc) (agregarPersonasYChecksum c2 sp) (checksumConMasAutores mc sp c1 c2)


-- lookupM: log N + addS: log N = 2log N
setConChecksum :: Map Persona (Set Checksum) -> Persona -> (Set Checksum) -> Checksum -> Set Checksum
setConChecksum mp p sc c = 
    case lookupM p mp of -- log N
        Just sc -> (addS sc c) -- log N
        Nothing -> (addS emptyS c) 


-- O(
    -- recursion de lista : p 
    -- log P : assocM 
    -- 2log P : setConChecksum
    -- p + 3log P = n log n
-- )
-- donde p es cada persona en la lista de personas
-- donde P es cada par persona:set en el mapa
agregarPersonasYChecksum :: Map Persona (Set Checksum) -> Checksum -> [Persona] -> Map Persona (Set Checksum)
agregarPersonasYChecksum mp _ [] = mp
agregarPersonasYChecksum mp c (p:ps) = assocM p (setConChecksum mp p sc c) (agregarPersonasYChecksum mp c ps)


-- Propósito: recibe un organizador y denota uno de los programas con más autores de todo ese organizador; denota
-- Nothing si no puede devolver un programa.
-- Eficiencia: O(1) en peor caso.

-- Esto puede requerir modificar el tipo de representación, agregar invariantes, y modificar operaciones existentes. Reescribir
-- sólo las operaciones que tienen cambios sustanciales y no en las que, por ejemplo, sólo se modifica un pattern matching.
elMayorPrograma :: Organizador -> Maybe Checksum
elMayorPrograma (MkO _ _ c) = c
