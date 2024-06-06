import Par

data Tree a = E | Leaf a | Join (Tree a) (Tree a)

mapreduce :: (a -> b) -> (b -> b -> b) -> b -> Tree a -> b
mapreduce f g e = mr
                  where mr E = e
                        mr (Leaf a) = f a
                        mr (Join l r) = let (l', r') = mr l ||| mr r
                                        in g l' r'

--a
max_tupla :: (Num a, Ord a) => (a, a, a, a) -> a
max_tupla (a, b, c, d) = max a $ max b $ max c d

tupla :: (Num a, Ord a) => a -> (a, a, a, a)
tupla v = let v' = max v 0 in (v', v', v', v)

calcular :: (Num a, Ord a) => (a, a, a, a) -> (a, a, a, a) -> (a, a, a, a)
calcular (subsec1, pref1, suf1, total1) (subsec2, pref2, suf2, total2) = let total = total1 + total2
                                                                             pref = max pref1 (total1 + pref2)
                                                                             suf = max suf2 (total2 + suf1)
                                                                             subsec = max subsec1 $ max subsec2 (suf1 + pref2)
                                                                         in (subsec, pref, suf, total)

mcss :: (Num a, Ord a) => Tree a -> a
mcss t = max_tupla (mapreduce tupla calcular (0, 0, 0, 0) t)

t1 = Join (Join (Join (Join (Leaf (-1)) (Leaf 1)) (Leaf 2)) (Leaf (-1))) (Leaf 1)