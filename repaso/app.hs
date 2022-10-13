sectores :: Nave -> Set SectorId
sectores n = sectores' (tripulantesN n) n

sectores' :: [Tripulante] -> Nave -> Set SectorId
sectores' [] n = emptyS
sectores' (t : ts) n =
  unionS
    (sectoresAsignados (nombre t) n)
    (sectores' ts n)

-- O(T log T) por tripulantesN
-- + T por sinSectores
-- => (T log T) + T => O(T (logT + 1))
sinSectoresAsignados :: Nave -> [Tripulante]
sinSectoresAsignados n = sinSectores (tripulantesN n)

-- O(n)
-- donde n es la cantidad de tripulantes dados
sinSectores [] = []
sinSectores (t:ts) =
  if isEmptyS (sectoresT t)
  then t : sinSectores ts
  else sinSectores ts
