import Par
import Data.Char

data BTree a = E | L a | N Int (BTree a) (BTree a) deriving Show

--stripSuﬁx〈’b’, ’a’, ’r’〉〈’f’, ’o’, ’o’, ’b’, ’a’, ’r’〉 = Just〈’f’, ’o’, ’o’〉
--stripSuﬁx〈’b’, ’a’, ’r’〉〈’f’, ’o’, ’o’, ’b’, ’a’, ’r’, ’r’〉 = Nothing

suf = N 3 (N 2 (L 'b') (L 'a')) (L 'r')
t1 = N 6 (N 2 (L 'f') (L 'o')) 
         (N 4 (N 2 (L 'o') (L 'b')) (N 2 (L 'a') (L 'r')))

t2 = N 7 (N 2 (L 'f') (L 'o')) 
         (N 5 (N 2 (L 'o') (L 'b')) (N 3 (L 'a') (N 2 (L 'r') (L 'r'))))

inorder :: BTree a -> [a]
inorder E = []
inorder (L x) = [x]
inorder (N _ l r) = l' ++ r' where (l', r') = inorder l ||| inorder r

cant :: BTree a -> Int
cant E = 0
cant (L a) = 1
cant (N n _ _) = n

mapReduce :: (a -> b) -> (b -> b -> b) -> b -> BTree a -> b
mapReduce f g e = mr
                    where
                      mr E = e
                      mr (L x) = f x
                      mr (N _ l r) = let (l', r') = mr l ||| mr l
                                     in g l' r'

splitN :: BTree a -> Int -> (BTree a, BTree a)
splitN E _ = (E, E)
splitN t 0 = (E, t)
splitN (L x) m = if m == 0 then (E, L x) else (L x, E)
splitN (N n l r) m | m >= cant l = let (l', r') = splitN r (m - cant l)
                                   in (N m l l', r')
                   | otherwise = let (l', r') = splitN l m
                                 in (l', N (n - m) r' r)

stripSufix :: Eq a => BTree a -> BTree a -> Maybe (BTree a)
stripSufix E t = Just t
stripSufix suf t | cant suf > cant t = Nothing
                 | otherwise = let n = cant suf
                                   m = cant t
                                   (prim, ult) = splitN t (m - n)
                                   (lult, lsuf) = inorder ult ||| inorder suf
                               in if lult == lsuf then Just prim else Nothing