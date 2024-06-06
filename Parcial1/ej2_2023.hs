type Interval = (Int, Int)
data ITree = E | N ITree Interval ITree deriving Show

it = N (N E (1, 3) E) (5, 9) (N E (10, 12) E)

right :: ITree -> Int
right (N _ (x, y) E) = y
right (N _ _ r) = right r

splitMax :: ITree -> (Interval, ITree)
splitMax (N _ (x, y) E) = ((x, y), E)
splitMax t@(N _ (x, y) r) = aux t E where aux (N l (x, y) E) t2 = ((x, y), t2)
                                          aux t@(N l (x, y) r) E = aux r (N l (x, y) E)
                                          aux t@(N l (x, y) r) t2@(N l2 (a, b) E) = aux r (N l2 (a, b) (N l (x, y) E))

merge :: ITree -> ITree -> ITree
merge t1 E = t1
merge E t2 = t2
merge t1@(N l1 i1 r1) t2@(N l2 i2 r2) = let (maxint, tree) = splitMax t1
                                        in N tree maxint t2

delElem :: ITree -> Int -> ITree
delElem E _ = E
delElem (N l (a, b) r) x | a == b = if x == a then merge l r else (if x < a then (N (delElem l x) (a, b) r) else (N l (a, b) (delElem r x)))
                         | a == x = N l (x + 1, b) r
                         | b == x = N l (a, x - 1) r
                         | a < x && x < b = N (N l (a, x - 1) E) (x + 1, b) r
                         | otherwise = if x < a then (N (delElem l x) (a, b) r) else (N l (a, b) (delElem r x))