data Investigacion
  = ConsI
      (Map Nombre Persona)
      (Map Evidencia [Nombre])
      (PriorityQueue Persona)
      Int

ingresarPersonas :: [Nombre] -> Investigacion -> Investigacion
ingresarPersonas [] i = i
ingresarPersonas (n:ns) i = ingresarPersonas ns (ingresarPersona n i)

ingresarPersona :: Nombre -> Investigacion -> Investigacion
ingresarPersona n (ConsI nm me pp n) =
  let persona = crearP n in
    ConsI (assocM n persona nm) me (insertPQ pp persona) n
