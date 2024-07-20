import Par

data BTree a = E | N Int (BTree a) a (BTree a) deriving Show

t1 = N 3 (N 1 E 'a' E) 'b' (N 1 E 'c' E)
t2 = N 4 (N 2 (N 1 E 'd' E) 'a' E) 'b' (N 1 E 'c' E)

cant :: BTree a -> Int
cant E = 0
cant (N n _ _ _) = n

nth :: BTree a -> Int -> a
nth (N n l x r) i | cant l == i = x
                  | cant l > i = nth l i
                  | cant l < i = nth r (i - cant l - 1)

cons :: a -> BTree a -> BTree a
cons x E = N 1 E x E
cons x (N n l y r) = N (n + 1) (cons x l) y r

tabulate :: (Int -> a) -> Int -> BTree a
tabulate f 1 = cons (f 0) E
tabulate f n = cons fn tab where (fn, tab) = f (n - 1) ||| tabulate f (n - 1)

mapT :: (a -> b) -> BTree a -> BTree b
mapT f E = E
mapT f (N n l x r) = let y = f x
                         (l', r') = mapT f l ||| mapT f r
                     in N n l' y r'

takeT :: Int -> BTree a -> BTree a
takeT _ E = E
takeT 0 t = E
takeT i t@(N n l x r) | i >= n = t
                      | i < n = if i > cant l then N i l x (takeT (i - cant l - 1) r)
                                              else takeT i l

dropT :: Int -> BTree a -> BTree a
dropT _ E = E
dropT 0 t = t
dropT i t@(N n l x r) | i >= n = E
                      | i < n = if i > cant l then dropT (i - cant l - 1) r
                                              else N (n - i) (dropT i l) x r