import Par

data Tree a = E | Leaf a | Join (Tree a) (Tree a) deriving Show

t :: Tree Int
t = Join (Join (Leaf 10) (Leaf 15)) (Leaf 20)

mapT :: (a -> b) -> Tree a -> Tree b
mapT f E = E
mapT f (Leaf a) = Leaf (f a)
mapT f (Join l r) = let (l', r') = mapT f l ||| mapT f r
                    in Join l' r'

reduceT :: (a -> a -> a) -> a -> Tree a -> a
reduceT f e E = e
reduceT f e (Leaf a) = a
reduceT f e (Join l r) = let (l', r') = reduceT f e l ||| reduceT f e r
                         in f l' r'

mapreduce :: (a -> b) -> (b -> b -> b) -> b -> Tree a -> b
mapreduce f g e = mr where
                      mr E = e
                      mr (Leaf a) = f a
                      mr (Join l r) = let (l', r') = mr l ||| mr r
                                      in g l' r'

join_r :: Tree a -> Tree a -> Tree a
join_r r E = r
join_r r x = Join x r

sufijos :: Tree Int -> Tree (Tree Int)
sufijos E = E
sufijos (Leaf _) = Leaf E
sufijos (Join l r) = let (l', r') = sufijos l ||| sufijos r
                     in Join (mapT (join_r r) l') r'

conSufijos :: Tree Int -> Tree (Int, Tree Int)
conSufijos t = zipT t (sufijos t)
                where
                  zipT (Leaf x) (Leaf y) = Leaf (x, y)
                  zipT (Join l r) (Join l' r') = let (l'', r'') = zipT l l' ||| zipT r r'
                                                 in Join l'' r''
                  zipT _ _ = E

maxT :: Tree Int -> Int
maxT = reduceT max 0

maxAII :: Tree (Tree Int) -> Int
maxAII = mapreduce maxT max 0

mejorGanancia :: Tree Int -> Int
mejorGanancia t = resta (conSufijos t)
                  where
                    resta (Leaf (x, y)) = mapreduce (\z -> z - x) max 0 y
                    resta (Join l r) = let (l', r') = resta l ||| resta r
                                       in max l' r'

                    --mapreduce resta max 0 (conSufijos t)