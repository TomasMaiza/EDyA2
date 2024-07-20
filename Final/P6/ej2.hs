import Par

data Tree a = E | Leaf a | Join (Tree a) (Tree a) deriving Show

t1 = Join (Join (Join (Join (Leaf (-1)) (Leaf 1)) (Leaf 2)) (Leaf (-1))) (Leaf 1)

mapreduce :: (a -> b) -> (b -> b -> b) -> b -> Tree a -> b
mapreduce f g e = mr
  where
    mr E = e
    mr (Leaf x) = f x
    mr (Join l r) = let (l', r') = mr l ||| mr r
                    in g l' r'

--a
mcss :: (Num a, Ord a) => Tree a -> a
mcss t = max_tupla (mapreduce tuplas maximo (0, 0, 0, 0) t)
        where
          tuplas v = let b = max v 0 in (b, b, b, v)
          maximo (sub1, pref1, suf1, tot1) (sub2, pref2, suf2, tot2) = let tot = tot1 + tot2
                                                                           sub = max sub1 $ max sub2 (suf1 + pref2)
                                                                           pref = max pref1 (tot1 + pref2)
                                                                           suf = max suf2 (suf1 + tot2)
                                                                       in (sub, pref, suf, tot)
          max_tupla (a, b, c, d) = max a $ max b $ max c d