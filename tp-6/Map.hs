module Map where

data Map k v = M [(k, v)]
  deriving Show

-- O(1)
emptyM :: Map k v
emptyM = M []

-- O(1)
simpleAssocM :: k -> v -> Map k v -> Map k v
simpleAssocM k v (M kvs) = M ((k, v) : kvs)

replaceValM :: Eq k => k -> v -> Map k v -> Map k v
replaceValM k v (M []) = M [(k, v)]
replaceValM k v (M (kv : kvs)) =
  let (kk, vv) = kv in
    if k == kk then
      M ((k,v) : kvs)
    else
      simpleAssocM kk vv $ replaceValM k v (M kvs)

assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v m =
  case lookupM k m of
    Nothing -> simpleAssocM k v m
    _       -> undefined

-- O(n)
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM _ (M []) = Nothing
lookupM k (M (kv : kvs)) =
  let (kk, vv) = kv
   in if k == kk then Just vv else lookupM k (M kvs)

-- O(n)
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (M (kv : kvs)) =
  let (kk, vv) = kv
   in if k == kk then M kvs else deleteM k (M (kv : kvs))

-- O(n)
keys :: Map k v -> [k]
keys (M []) = []
keys (M (kv : kvs)) =
  let (k, v) = kv
   in k : keys (M kvs)
