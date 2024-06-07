import Par

data T a = E | N (T a) a (T a) deriving Show

t1 :: T Int
t1 = N (N E 10 (N E 14 E)) 5 (N (N (N E 4 E) 12 E) 9 (N E 10 E))

t2 :: T Int
t2 = N (N E 22 E) 25 (N (N E 29 E) 24 (N E 27 E))

altura :: T a -> Int
altura E = 0
altura (N l x r) = let (l', r') = altura l ||| altura r
                   in 1 + (max l' r')

--a
combinar :: T a -> T a -> T a
combinar t1 E = t1
combinar E t2 = t2
combinar t1@(N l x r) t2 = N (combinar l r) x t2

--b
filterT :: (a -> Bool) -> T a -> T a
filterT p E = E
filterT p (N l x r) = let (l', r') = filterT p l ||| filterT p r
                      in if p x then N l' x r' else combinar l' r'

--c
quicksortT :: T Int -> T Int
quicksortT E = E
quicksortT t@(N l x r) = let (menorx, mayorx) = filterT (<= x) t ||| filterT (> x) t
                             (qmenor, qmayor) = quicksortT menorx ||| quicksortT mayorx
                         in combinar qmenor qmayor