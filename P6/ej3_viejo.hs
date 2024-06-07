data Tree a = E | Leaf a | Join (Tree a) (Tree a) deriving Show

(|||) :: a -> b -> (a, b)
a ||| b = (a, b)

t :: Tree Int
t = Join (Join (Leaf 10) (Leaf 15)) (Leaf 20)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f E = E
mapTree f (Leaf a) = Leaf (f a)
mapTree f (Join l r) = Join fl fr where (fl, fr) = mapTree f l ||| mapTree f r

join_r :: Tree a -> Tree a -> Tree a
join_r r E = r
join_r r x = Join x r

sufijos :: Tree Int -> Tree (Tree Int)
sufijos E = E
sufijos (Leaf _) = Leaf E
sufijos (Join l r) = Join (mapTree (join_r r) sl) sr where (sl, sr) = sufijos l ||| sufijos r

aux :: Tree (Tree Int) -> Tree Int -> Tree (Int, Tree (Tree Int))
aux sufijos E = E
aux sufijos (Leaf a) = Leaf (a, sufijos)
aux (Join ls rs) (Join l r) = Join l' r' where (l', r') = aux ls l ||| aux rs r

aux2 :: Tree (Tree Int) -> Tree Int
aux2 (Leaf E) = E
aux2 (Leaf x) = x
aux2 (Join l r) =  

conSufijos :: Tree Int -> Tree (Int, Tree Int)
conSufijos t = aux (sufijos t) t

mejorGanancia :: Tree Int -> Int
mejorGanancia = undefined







sufijos_lindo :: Tree Int -> Tree Int
sufijos_lindo E = E
sufijos_lindo (Leaf x) = Leaf x
sufijos_lindo (Join l r) = Join (mapTree (join_r r) sl) sr where (sl, sr) = sufijos l ||| sufijos r