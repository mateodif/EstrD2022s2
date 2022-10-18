module RAList where

set :: Ord a => Int -> a -> RAList a -> RAList a
set i v (MkR l mp h) =
  let vj = fromJust (lookupM mp i) in
    mkR l (assocM i v mp) (reemplazar vj nv h)

-- Precondicion: existe el elemento
reemplazar :: Ord a => a -> a -> Heap a -> Heap a
reemplazar vj nv h =
  if findMin h == vj
  then insertH nv (deleteMin h)
  else insertH (findMin h) (reemplazar vj nv (deleteMinH h))

addAt :: Ord a => Int -> a -> RAList a -> RAList a
addAt i nv (mkR t mp h) =
  mkR (t+1) (assocM i nv (desplazarK i t mp)) (insertH nv h)

desplazarK :: Ord a => Int -> Int -> Map Int a -> Map Int a
desplazarK i t mp =
  if i == t
  then mp
  else assocM (i+1) (fromJust (lookupM mp i)) (desplazarK (i+1) t mp)

remove :: Ord a => a -> a -> Heap a -> Heap a
remove vj nv h =
  if findMin h == vj
  then (deleteMin h)
  else insertH (findMin h) (reemplazar vj nv (deleteMinH h))
