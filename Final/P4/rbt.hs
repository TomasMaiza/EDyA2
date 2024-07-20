a ||| b = (a, b)

data Color = R | B deriving Show
data RBT a = E | T Color (RBT a) a (RBT a) deriving Show

rbt = T B (T B (T R E 1 E) 2 E) 3 (T R (T B (T R E 4 E) 5 E) 7 (T B E 8 E))

memberRBT :: Ord a => a -> RBT a -> Bool
memberRBT a E = False
memberRBT a (T _ l b r) | a == b = True
                        | a < b = memberRBT a l
                        | otherwise = memberRBT a r

balance :: Color -> RBT a -> a -> RBT a -> RBT a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r

insertRBT :: Ord a => a -> RBT a -> RBT a
insertRBT x t = makeBlack (ins x t) where
                                      ins x E = T R E x E
                                      ins x (T c l y r) | x < y = balance c (ins x l) y r
                                                        | x > y = balance c l y (ins x r)
                                                        | otherwise = T c l y r
                                      makeBlack E = E
                                      makeBlack (T _ l x r) = T B l x r

-- EJ5
--1
data OTTtree a = H 
               | N2 a (OTTtree a) (OTTtree a) 
               | N3 a a (OTTtree a) (OTTtree a) (OTTtree a)
               | N4 a a a (OTTtree a) (OTTtree a) (OTTtree a) (OTTtree a) deriving Show

--2
rbt2ott :: Ord a => RBT a -> OTTtree a
rbt2ott E = H
rbt2ott (T B (T R l x r) y (T R l' z r')) = N4 x y z l1 r1 l2 r2 where (l1, r1) = rbt2ott l ||| rbt2ott r
                                                                       (l2, r2) = rbt2ott l' ||| rbt2ott r'
rbt2ott (T B (T R l x r) y r') = N3 x y l1 r1 (rbt2ott r') where (l1, r1) = rbt2ott l ||| rbt2ott r
rbt2ott (T B l' x (T R l y r)) = N3 x y (rbt2ott l') l1 r1 where (l1, r1) = rbt2ott l ||| rbt2ott r
rbt2ott (T B l x r) = N2 x l1 r1 where (l1, r1) = rbt2ott l ||| rbt2ott r
