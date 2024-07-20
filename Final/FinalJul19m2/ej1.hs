import Par

data BTree a = E | L a | N Int (BTree a) (BTree a) deriving Show

t1 = N 7 (N 5 (N 3 (L 'a') (N 2 (L 'b') (L 'c'))) (N 2 (L 'd') (N 1 E (L 'e')))) (N 2 (L 'f') (L 'g'))

--a
cant :: BTree a -> Int
cant E = 0
cant (L a) = 1
cant (N n _ _) = n

aux :: (a -> Int -> b) -> BTree a -> Int -> BTree b
aux f E _ = E
aux f (L x) i = L (f x i)
aux f (N n l r) i = let cl = cant l
                        (l', r') = aux f l i ||| aux f r (i + cl)
                      in N n l' r'

mapIndex :: (a -> Int -> b) -> BTree a -> BTree b
mapIndex f E = E
mapIndex f (L x) = L (f x 0)
mapIndex f t@(N n l r) = aux f t 0

--b
--fromSlow n 10 3 = 〈n, n, n, n + 1, n + 1, n + 1, n + 2, n + 2, n + 2, n + 3〉
--fromSlow n 10 1 = 〈n, n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7, n + 8, n + 9〉

from_aux :: Int -> Int -> Int -> Int -> BTree Int
from_aux n m 0 k = from_aux (n + 1) m k k

tabulate :: (Int -> a) -> Int -> BTree a
tabulate f 0 = E
tabulate f 1 = L (f 0)
tabulate f n = N n (L (f (n-1))) (tabulate f (n - 1))

fromSlow :: Int -> Int -> Int -> BTree Int
fromSlow n 0 k = E
fromSlow n m k = let t = tabulate (\_ -> n) m
                 in mapIndex (\n i -> n + div i k) t