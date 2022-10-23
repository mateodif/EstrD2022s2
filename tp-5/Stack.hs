module Stack where

data Stack a = S [a]
  deriving Show

emptyS :: Stack a
emptyS = S []

isEmptyS :: Stack a -> Bool
isEmptyS (S []) = True
isEmptyS _      = False

push :: a -> Stack a -> Stack a
push a (S l) = S (a:l)

top :: Stack a -> a
top (S l) = head l

pop :: Stack a -> Stack a
pop (S l) = S (tail l)

lenS :: Stack a -> Int
lenS (S l) = length l
