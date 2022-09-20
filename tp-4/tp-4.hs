data Lobo = Cr Nombre
          | Ex Nombre [Territorio] Lobo Lobo
          | Ca Nombre [Presa] Lobo Lobo Lobo

ept :: Manada -> [(Territorio, [Nombre])]

eptL :: Lobo -> [(Territorio, [Nombre])]
eptL (Cr n) = []
eptL (Ex n ts l1 l2) = combinarEpt (mkListaDePares ts [n]) (combinarEpt (eptL l1) (eptL l2))
eptL (Ca n ps l1 l2 l3) = combinarEpt (eptL l1) (combinarEpt (eptL l2) (eptL l3))

combinarEpt :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
combinarEpt []     ys = ys
combinarEpt (x:xs) ys = agregarNs x (combinarEpt ys)

agregarNs :: (Territorio, [Nombre]) -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarNs (t,ns) [] = [(t,ns)]
agregarNs (t,ns) ((t',ns'):xs) = if t == t'
                                    then (t, ns ++ ns') : xs
                                    else (t',ns') : agregarNs (t,ns) xs

mkListaDePares :: [a] -> b -> [(a,b)]
