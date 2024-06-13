import Par

data Tree a = E | N Int (Tree a) a (Tree a) deriving Show

tam :: Tree a -> Int
tam E = 0
tam (N n _ _ _) = n

filterPrefix :: (a -> Bool) -> Tree a -> Tree a
filterPrefix p E = E
filterPrefix p (N n E x r) = if p x then let r' = filterPrefix p r in N (tam r' + 1) E x r' 
                                    else E
filterPrefix p (N n l x r) = let (l', r') = filterPrefix p l ||| filterPrefix p r
                                 m = tam l' + tam r' + 1 
                             in case l' of
                                E -> E
                                _ -> if p x && (tam l == tam l') then N m l' x r' else l'


inorder :: Tree a -> [a]
inorder E = []
inorder (N _ l x r) = let (l', r') = inorder l ||| inorder r
                      in l' ++ [x] ++ r'

s1 = N 6 (N 2 (N 1 E 6 E) 6 E) 8 (N 3 (N 1 E 1 E) 4 (N 1 E 5 E))
s2 = N 3 (N 1 E 1 E) 4 (N 1 E 5 E)
s3 = N 4 E 8 (N 3 (N 1 E 4 E) 6 (N 1 E 5 E))
s4 = N 4 (N 3 (N 1 E 8 E) 4 (N 1 E 5 E)) 6 E