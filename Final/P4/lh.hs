a ||| b = (a, b)

type Rank = Int
data Heap a = E | N Rank a (Heap a) (Heap a) deriving Show

rank :: Heap a -> Rank
rank E = 0
rank (N r _ _ _) = r

makeH :: a -> Heap a -> Heap a -> Heap a
makeH x a b = if rank a >= rank b then N (rank b + 1) x a b
                                  else N (rank a + 1) x b a

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 E = h1
merge E h2 = h2
merge h1@(N _ x a1 b1) h2@(N _ y a2 b2) = if x <= y then makeH x a1 (merge b1 h2)
                                                    else makeH y a2 (merge h1 b2)

insertLH :: Ord a => a -> Heap a -> Heap a
insertLH x h = merge (N 1 x E E) h

findMin :: Ord a => Heap a -> a
findMin (N _ x _ _) = x

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (N _ x a b) = merge a b

-- EJ 6
fromList :: Ord a => [a] -> Heap a
fromList [] = E
fromList (x:xs) = insertLH x (fromList xs)

-- EJ 7
data PHeaps a = Empty | Root a [PHeaps a] deriving Show

--1
isPHeap :: Ord a => PHeaps a -> Bool
isPHeap Empty = True
isPHeap (Root x []) = True
isPHeap (Root x xs) = let mayor x (Root y ys) = if x > y then True else False
                          mayores = filter (mayor x) xs
                      in case mayores of
                        [] -> and (map isPHeap xs) 
                        _ -> False

--2
mergePH :: Ord a => PHeaps a -> PHeaps a -> PHeaps a
mergePH Empty ph = ph
mergePH ph Empty = ph
mergePH p1@(Root x xs) p2@(Root y ys) = if x < y then Root x (p2 : xs) else Root y (p1 : ys)

--3
insertPH :: Ord a => PHeaps a -> a -> PHeaps a
insertPH ph x = mergePH ph (Root x [])

--4
concatHeaps :: Ord a => [PHeaps a] -> PHeaps a
concatHeaps [] = Empty
concatHeaps (x:xs) = mergePH x (concatHeaps xs)
--concatHeaps (x:xs) = case x of
--                     Empty -> concatHeaps xs
--                      Root y ys -> mergePH (insertPH (concatHeaps ys) y) (concatHeaps xs)

--5
delMin :: Ord a => PHeaps a -> Maybe (a, PHeaps a)
delMin Empty = Nothing
delMin (Root x xs) = Just (x, concatHeaps xs)