import Par
import Seq
import ListSeq

data BTree a = E | L a | N Int (BTree a) (BTree a)

aux :: Eq a => BTree a -> BTree a -> BTree a -> Maybe (BTree a)
aux suf E _ = Nothing
aux suf (L a) t = 

stripSufix :: Eq a => BTree a -> BTree a -> Maybe (BTree a)
stripSufix E _ = E
stripSufix _ E = Nothing
stripSufix suf@(N m  _ _) t@(N n l r) | m == n = comparar suf t
                                      | m > n = stripSufix