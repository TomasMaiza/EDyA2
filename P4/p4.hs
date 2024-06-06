module P4 where
import Data.List

-- EJ 1

data Tree a = Hoja | Nodo (Tree a) a (Tree a) deriving Show

--a
completo :: a -> Int -> Tree a
completo _ 0 = Hoja
completo x d = Nodo (completo x h) x (completo x h) where h = d - 1

--b
balanceado :: a -> Int -> Tree a
balanceado x n | n < 1 = Hoja
               | otherwise = Nodo (balanceado x (n - 1)) x (balanceado x (n - 2))

-- EJ 2

type BST a = Tree a

bst :: BST Int
bst = Nodo (Nodo Hoja 0 Hoja) 3 (Nodo (Nodo Hoja 5 (Nodo Hoja 7 Hoja)) 8 Hoja)

bst2 :: BST Int
bst2 = Nodo (Nodo (Nodo Hoja 2 Hoja) 3 (Nodo Hoja 4 Hoja)) 9 (Nodo Hoja 11 (Nodo Hoja 15 Hoja))

minimumBST :: Ord a => BST a -> a
minimumBST (Nodo Hoja a r) = a
minimumBST (Nodo l a r) = minimumBST l

memberBST :: Ord a => a -> BST a -> Bool
memberBST a Hoja = False
memberBST a (Nodo l b r) | a == b = True
                         | a < b = memberBST a l
                         | a > b = memberBST a r

insertBST :: Ord a => a -> BST a -> BST a
insertBST a Hoja = Nodo Hoja a Hoja
insertBST a (Nodo l b r) | a <= b = Nodo (insertBST a l) b r
                         | otherwise = Nodo l b (insertBST a r)

--1
maximumBST :: Ord a => BST a -> a
maximumBST (Nodo l a Hoja) = a
maximumBST (Nodo l a r) = maximumBST r

--2

checkBST :: Ord a => BST a -> Bool
checkBST Hoja = True
checkBST (Nodo Hoja a Hoja) = True
checkBST (Nodo l a Hoja) = a >= maximumBST l && checkBST l
checkBST (Nodo Hoja a r) = a < minimumBST r && checkBST r
checkBST (Nodo l a r) = a >= maximumBST l && checkBST l && a < minimumBST r && checkBST r

--3
splitBST :: Ord a => BST a -> a -> (BST a, BST a)
splitBST Hoja _ = (Hoja, Hoja)
splitBST (Nodo l a r) x | a <= x = (Nodo l a izq2, der2)
                        | otherwise = (izq1, Nodo der1 a r)
                        where
                          (izq1, der1) = splitBST l x
                          (izq2, der2) = splitBST r x

--4
join :: Ord a => BST a -> BST a -> BST a
join Hoja t = t
join t Hoja = t
join (Nodo l a r) t = Nodo (join l izq) a (join r der) 
                          where
                            (izq, der) = splitBST t a

-- EJ 3

memberaux :: Ord a => a -> a -> BST a -> Bool
memberaux c x Hoja = c == x
memberaux c x (Nodo l a r) | x <= a = memberaux a x l
                           | otherwise = memberaux c x r

memberBST2 :: Ord a => a -> BST a -> Bool
memberBST2 x Hoja = False
memberBST2 x t@(Nodo l a r) = memberaux a x t

-- EJ 4
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

--a
lbalance :: Color -> RBT a -> a -> RBT a -> RBT a
lbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
lbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
lbalance c l a r = T c l a r

rbalance :: Color -> RBT a -> a -> RBT a -> RBT a
rbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rbalance c l a r = T c l a r

--b
insertRBT2 :: Ord a => a -> RBT a -> RBT a
insertRBT2 x t = makeBlack(ins x t) where
                                      ins x E = T R E x E
                                      ins x (T c l y r) | x < y = lbalance c (ins x l) y r
                                                        | x > y = rbalance c l y (ins x r)
                                                        | otherwise = T c l y r
                                      makeBlack E = E
                                      makeBlack (T _ l x r) = T B l x r

-- EJ 5

--1
data Tree123 a = H | Nodo2 a (Tree123 a) (Tree123 a) 
                   | Nodo3 a a (Tree123 a) (Tree123 a) (Tree123 a) 
                   | Nodo4 a a a (Tree123 a) (Tree123 a) (Tree123 a) (Tree123 a) deriving Show

--2
min2max :: Ord a => a -> a -> a -> (a, a, a)
min2max x y z | x <= y = if y <= z then (x, y, z) else (x, z, y)
              | y <= z = if z <= x then (y, z, x) else (y, x, z)
              | z <= x = if x <= y then (z, x, y) else (z, y, x)

tree123torbt :: Ord a => Tree123 a -> RBT a
tree123torbt H = E
tree123torbt (Nodo2 x h1 h2) = T B (tree123torbt h1) x (tree123torbt h2)
tree123torbt (Nodo3 x y h1 h2 h3) | x >= y = T B (T R (tree123torbt h1) y (tree123torbt h2)) x (tree123torbt h3)
                                  | otherwise = T B (tree123torbt h1) x (T R (tree123torbt h2) y (tree123torbt h3))
tree123torbt (Nodo4 x y z h1 h2 h3 h4) = T B (T R (tree123torbt h1) a (tree123torbt h2)) b (T R (tree123torbt h3) c (tree123torbt h4)) where (a, b, c) = min2max x y z

{-
data Color = R | B deriving Show
data RBT a = E | T Color (RBT a) a (RBT a) deriving Show
-}

rbttotree123 :: Ord a => RBT a -> Tree123 a
rbttotree123 E = H
rbttotree123 (T B (T R l1 x r1) y (T R l2 z r2)) = Nodo4 x y z (rbttotree123 l1) (rbttotree123 r1) (rbttotree123 l2) (rbttotree123 r2)
rbttotree123 (T B (T R l x r) y r') = Nodo3 x y (rbttotree123 l) (rbttotree123 r) (rbttotree123 r')
rbttotree123 (T B l' x (T R l y r)) = Nodo3 x y (rbttotree123 l') (rbttotree123 l) (rbttotree123 r)
rbttotree123 (T B l x r) = Nodo2 x (rbttotree123 l) (rbttotree123 r)

-- EJ 6
type Rank = Int
data Heap a = Empty | N Rank a (Heap a) (Heap a) deriving Show

rank :: Heap a -> Rank
rank Empty = 0
rank (N r _ _ _) = r

makeH :: a -> Heap a -> Heap a -> Heap a
makeH x a b = if rank a >= rank b then N (rank b + 1) x a b
                                  else N (rank a + 1) x b a

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 Empty = h1
merge Empty h2 = h2
merge h1@(N _ x a1 b1) h2@(N _ y a2 b2) = if x <= y then makeH x a1 (merge b1 h2)
                                                    else makeH y a2 (merge h1 b2)

insertLH :: Ord a => a -> Heap a -> Heap a
insertLH x h = merge (N 1 x Empty Empty) h

findMin :: Ord a => Heap a -> a
findMin (N _ x _ _) = x

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (N _ x a b) = merge a b

fromList :: Ord a => [a] -> Heap a
fromList [] = Empty
fromList (x:xs) = insertLH x (fromList xs)

-- EJ 7

data PHeaps a = Empty1 | Root a [PHeaps a] deriving Show

--1
menorQueHijos :: Ord a => a -> [PHeaps a] -> Bool
menorQueHijos a [] = True
menorQueHijos a ((Root y ys):xs) = a < y && menorQueHijos a xs

isPHeap :: Ord a => PHeaps a -> Bool
isPHeap Empty1 = True
isPHeap (Root a xs) = (menorQueHijos a xs) && and(map isPHeap xs)

--2
mergePH :: Ord a => PHeaps a -> PHeaps a -> PHeaps a
mergePH Empty1 ph2 = ph2
mergePH ph1 Empty1 = ph1
mergePH ph1@(Root x xs) ph2@(Root y ys) | x <= y = Root x (ph2:xs)
                                        | otherwise = Root y (ph1:ys)

--3
insertPH :: Ord a => PHeaps a -> a -> PHeaps a
insertPH ph x = mergePH ph (Root x [])

--4
concatHeaps :: Ord a => [PHeaps a] -> PHeaps a
concatHeaps [] = Empty1
concatHeaps (x:xs) = mergePH x (concatHeaps xs)

--5
delMin :: Ord a => PHeaps a -> Maybe (a, PHeaps a)
delMin Empty1 = Nothing
delMin (Root x xs) = Just (x, concatHeaps xs)