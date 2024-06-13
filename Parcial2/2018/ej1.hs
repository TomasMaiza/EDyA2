import Par

data Tree key val = E | N Int (Tree key val) (key, val) (Tree key val) deriving Show

tam :: Tree key val -> Int
tam E = 0
tam (N n _ _ _) = n

t1 = N 6 (N 1 E (2, 'b') E) (3, 'a') (N 4 (N 2 E (4, 'd') (N 1 E (6, 'e') E)) (8, 'c') (N 1 E (12, 'f') E))

--a
splitMax :: Tree k v -> ((k, v), Tree k v)
splitMax (N _ l x E) = (x, l)
splitMax (N n l x r) = let (y, r') = splitMax r
                       in (y, N (n - 1) l x r')


  
  
  {-let (t', m) = delete_aux k t
                             n' = n - m
                         in case t' of
                            E -> E
                            N y l x r -> N n' l x r 
              where
                delete_aux k E = (E, 0)
                delete_aux k (N y l (q, v) r) | k == q = -}