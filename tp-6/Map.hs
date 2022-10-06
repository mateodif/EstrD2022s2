module Map where

data Map k v = M [(k, v)]

-- O(1)
emptyM :: Map k v
emptyM = M []

-- O(1)
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M kvs) = M ((k, v) : kvs)

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
