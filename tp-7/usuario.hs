data Tree a
  = EmptyT
  | NodeT a (Tree a) (Tree a)
  deriving (Show)

esBST :: Ord a => Tree a -> Bool
esBST EmptyT = True
esBST (NodeT x l r) =
  esMayorATodo x l && esMenorATodo x r && esBST l && esBST r

esMayorATodo :: Ord a => a -> Tree a -> Bool
esMayorATodo a EmptyT = True
esMayorATodo a (NodeT x l r) = a > x && esMayorATodo a l && esMayorATodo a r

esMenorATodo :: Ord a => a -> Tree a -> Bool
esMenorATodo a EmptyT = True
esMenorATodo a (NodeT x l r) = a < x && esMayorATodo a l && esMayorATodo a r

-- si es BST    => O(log n)
-- si no es BST => O(n)
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
elMaximoMenorA e EmptyT = Nothing
elMaximoMenorA e (NodeT x l r) =
  if x < e
  then elegirMax x (elMaximoMenorA e r)
  else elMaximoMenorA e l

elegirMax :: Ord a => a -> Maybe a -> Maybe a
elegirMax a Nothing  = Just a
elegirMax a (Just v) = Just (max a v)


-- agregarEmpleado: precondicion, el empleado no debe estar en la empresa
