data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show

t = Node 3 (Node 1 Empty 'a' Empty) 'b' (Node 1 Empty 'c' Empty)
t2 = Node 4 (Node 2 Empty 'a' (Node 1 Empty 'd' Empty)) 'b' (Node 1 Empty 'c' Empty)

a ||| b = (a, b)
 
--a
cant :: BTree a -> Int
cant Empty = 0
cant (Node x _ _ _) = x

nth :: BTree a -> Int -> a
nth (Node n l x r) i | cl + 1 == i = x
                     | i <= cl = nth l i
                     | otherwise = nth r (i - cl - 1)
                      where
                        cl = cant l


-- b
inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node _ l x r) = ordl ++ [x] ++ ordr where (ordl, ordr) = inorder l ||| inorder r

cons :: a -> BTree a -> BTree a
cons x Empty = Node 1 Empty x Empty
cons x t@(Node n l d r) = Node (n + 1) Empty x t
--cons x (Node n l d r) = Node (n+1) (cons x l) d r

-- c
tabulate :: (Int -> a) -> Int -> BTree a
tabulate f 0 = Empty
tabulate f n = cons efe tabu where (efe, tabu) = (f (n - 1)) ||| (tabulate f (n-1))


-- d
mapT :: (a -> b) -> BTree a -> BTree b
mapT f Empty = Empty
mapT f (Node x l d r) = let (ml, mr) = (mapT f l) ||| (mapT f r)
                        in Node x ml (f d) mr


-- e
take' :: Int -> BTree a -> BTree a
take' _ Empty = Empty
take' 0 _ = Empty
take' n nod@(Node x l d r) | n >= x = nod
                           | otherwise = let cl = cant l
                                             (pl, pr) = take' (n-1) l ||| take' (n - cl - 1) r
                                         in  (Node (1 + (cant pl) + (cant pr)) pl d pr)

{-
-- f
drop :: Int -> BTree a -> BTree a
drop n nod@(Node x l d r) | n < x = nod
                          | otherwise = let cl = cant l
                                            (pl, pr) = drop (n-1) l ||| drop (n - cl - 1) r
                                        in  (Node (1 + (cant pl) + (cant pr)) pl d pr) -}