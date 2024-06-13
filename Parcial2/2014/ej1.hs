import Par

data Tree a = E | L a | N Int (Tree a) (Tree a) deriving Show

mapReduce :: (a -> b) -> (b -> b -> b) -> b -> Tree a -> b
mapReduce f combine e = mr
                        where
                          mr E = e
                          mr (L x) = f x
                          mr (N _ l r) = let (l', r') = mr l ||| mr r
                                         in combine l' r' 

size :: Tree a -> Int
size E = 0
size (L _) = 1
size (N n _ _) = n

inorder :: Tree a -> [a]
inorder E = []
inorder (L a) = [a]
inorder (N _ l r) = let (ls, rs) = inorder l ||| inorder r
                    in ls ++ rs

s1 = N 3 (N 2 (L 5) (L 2)) (L 3)
s2 = L 6
s3 = N 2 (L 8) (L 0)
s4 = N 3 (N 2 (L s1) (L s2)) (L s3)

--a
concat' :: Tree (Tree a) -> Tree a
concat' E = E
concat' (L t) = t
concat' (N n l r) = let (l', r') = concat' l ||| concat' r
                    in N (size l' + size r') l' r'

{-
concat'' :: Tree (Tree a) -> Tree a
concat'' t = mapReduce f g E t
              where
                f (L t') = t' 

                g l r = N (size l + size r) l r -}

--b

s5 = N 6 (N 3 (N 2 (L 1) (L 2)) (L 3)) (N 3 (N 2 (L 4) (L 5)) (L 6))

subsequence :: Tree a -> Int -> Int -> Tree a
subsequence E i j = E
subsequence (L x) i j = if i == 0 then L x else E
subsequence t@(N n l r) i j = if i > n || i < 0 then E else subseq (j - i + 1)
                                where
                                  subseq cant | cant >= n = t
                                              | otherwise = let (subl, subr) = subsequence l (n - i) (n - j) ||| subsequence r (n - i) (n - j)
                                                            in N cant subl subr