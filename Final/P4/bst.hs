a ||| b = (a, b)

data BST a = H | N (BST a) a (BST a) deriving Show

t1 :: BST Int
t1 = N (N H 0 H) 3 (N (N H 5 (N H 7 H)) 8 H)

t2 :: BST Int
t2 = N (N (N H 2 H) 3 (N H 4 H)) 9 (N H 11 (N H 15 H))

minimumBST :: Ord a => BST a -> a
minimumBST (N H a r) = a
minimumBST (N l a r) = minimumBST l

memberBST :: Ord a => a -> BST a -> Bool
memberBST a H = False
memberBST a (N l b r) | a == b = True
                      | a < b = memberBST a l
                      | a > b = memberBST a r

insertBST :: Ord a => a -> BST a -> BST a
insertBST a H = N H a H
insertBST a (N l b r) | a <= b = N (insertBST a l) b r
                      | otherwise = N l b (insertBST a r)

-- EJ 2
--3
splitBST :: Ord a => BST a -> a -> (BST a, BST a)
splitBST H _ = (H, H)
splitBST (N l a r) x | a <= x = (N l a izq2, der2)
                     | otherwise = (izq1, N der1 a r)
                        where
                          (izq1, der1) = splitBST l x
                          (izq2, der2) = splitBST r x

--4
join :: Ord a => BST a -> BST a -> BST a
join H t = t
join t H = t
join (N l x r) t = let (tl, tr) = splitBST t x
                       (l', r') = join l tl ||| join r tr
                   in N l' x r'