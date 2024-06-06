data Tree a = E | Leaf a | Join (Tree a) (Tree a) deriving Show

a ||| b = (a, b)

t1 = Join (Join (Join (Join (Leaf (-1)) (Leaf 1)) (Leaf 2)) (Leaf (-1))) (Leaf 1)

mapreduce :: (a -> b) -> (b -> b -> b) -> b -> Tree a -> b
mapreduce f g e = mr where mr E = e
                           mr (Leaf a) = f a
                           mr (Join l r) = let (l', r') = mr l ||| mr r
                                           in g l' r'

reduce f e = mr where mr E = e
                      mr (Leaf a) = a
                      mr (Join l r) = let (l', r') = mr l ||| mr r
                                      in f l' r'

max_tupla :: (Ord a) => (a, a, a, a) -> a
max_tupla (a, b, c, d) = max a $ max b $ max c d

reemplazar :: (Ord a, Num a) => a -> (a, a, a, a)
reemplazar v = (m, m, m, v) where m = max v 0

calcular_sumas :: (Ord a, Num a) => (a, a, a, a) -> (a, a, a, a) -> (a, a, a, a)
calcular_sumas (sumaContigua1, pref1, suf1, suma1) (sumaContigua2, pref2, suf2, suma2) = (a,b,c,d)
                                                                                         where
                                                                                            a = max sumaContigua1 $ max sumaContigua2 (suf1 + pref2)
                                                                                            b = max pref1 (suma1 + pref2) 
                                                                                            c = max suf2 (suf1 + suma2)
                                                                                            d = suma1 + suma2 

mcss :: (Num a, Ord a) => Tree a -> a
mcss t = max_tupla (mapreduce reemplazar calcular_sumas (0, 0, 0, 0) t)