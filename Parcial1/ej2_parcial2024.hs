data Color = R | B deriving (Eq, Show)
data AATree a = N Color a (AATree a) (AATree a) | E deriving (Eq, Show)

isBST :: Ord a => AATree a -> Bool
isBST E = True
isBST (N _ _ E E) = True
isBST (N _ x l E) = x >= max' l && isBST l
isBST (N _ x E r) = x <= min' r && isBST r
isBST (N _ x l r) = x >= max' l && isBST l && x <= min' r && isBST r

min' :: AATree a -> a
min' (N _ x E _) = x
min' (N _ x l _) = min' l

max' :: AATree a -> a
max' (N _ x _ E) = x
max' (N _ x _ r) = max' r

isAATree :: Ord a => AATree a -> Bool
isAATree E = True
isAATree t = isBST t && isAATree' t

isAATree' :: Ord a => AATree a -> Bool
isAATree' E = True
isAATree' (N _ x E E) = True
isAATree' (N c x l E) = False
isAATree' (N c x E r) = isAATree' r && (if c == R then isBlack r else True) && alturaNegra r == 0
isAATree' (N c x l r) = isBlack l && isAATree' l && isAATree' r && (if c == R then isBlack r else True) && alturaNegra l == alturaNegra r

isBlack :: AATree a -> Bool
isBlack (N c _ _ _) = c == B

alturaNegra :: AATree a -> Int
alturaNegra E = 0
alturaNegra (N c _ l r) = case c of
                          B -> 1 + maxi(alturaNegra l, alturaNegra r)
                          R -> maxi(alturaNegra l, alturaNegra r)

maxi :: (Int, Int) -> Int
maxi (a, b) = if a >= b then a else b

insert :: Ord a => a -> AATree a -> AATree a
insert a E = N B a E E
insert a t = makeBlack(balance(ins a t)) where
                                          ins a E = N R a E E
                                          ins a (N c x l r) | a < x = balance (N c x (ins a l) r)
                                                            | a > x = balance (N c x l (ins a r))
                                                            | otherwise = N c x l r
                                          makeBlack (N _ x l r) = N B x l r

balance :: AATree a -> AATree a
balance E = E
balance t = (split . skew) t where
                              skew (N c' y (N r x a b) c) = N c' x a (N R y b c)
                              skew t = t
                              split (N B x a (N R y b (N R z c d))) = N R y (N B x a b) (N B z c d)
                              split t = t

t = N B 4 
    (N B 2 (N B 1 E E) (N B 3 E E)) 
    (N R 11 (N B 8 (N B 5 E E) (N B 9 E E)) 
    (N B 15 (N B 13 E (N R 14 E E)) (N R 20 (N B 19 E E) (N B 21 E E))))