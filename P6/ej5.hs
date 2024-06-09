import Par
data BTree a = Empty | Node Int (BTree a) a (BTree a) deriving Show

t = Node 3 (Node 1 Empty 'a' Empty) 'b' (Node 1 Empty 'c' Empty)
t2 = Node 4 (Node 2 Empty 'a' (Node 1 Empty 'd' Empty)) 'b' (Node 1 Empty 'c' Empty)

-- Ejemplo 1234567
nodo5 = Node 7 nodo4 5 nodo7
nodo4 = Node 4 nodo2 4 Empty
nodo2 = Node 3 nodo1 2 nodo3
nodo1 = Node 1 Empty 1 Empty
nodo3 = Node 1 Empty 3 Empty
nodo7 = Node 2 nodo6 7 Empty
nodo6 = Node 1 Empty 6 Empty

cant :: BTree a -> Int
cant Empty = 0
cant (Node x _ _ _) = x

--a
splitAt' :: BTree a -> Int -> (BTree a, BTree a)
splitAt' Empty _ = (Empty, Empty)
splitAt' t@(Node n l x r) i | n <= i = (t, Empty)
                            | n > i && i >= cant l = let (l', r')  = splitAt' l i
                                                     in (l', Node (n - i) r' x r)
                            | n > i && i < cant l = let (l', r')  = splitAt' r (i - cant l)
                                                    in (Node i l x l', r')

