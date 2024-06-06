module Ej5P2 where
import Data.List
import Data.Char

--a
f x = let (y, z) = (x, x) in y

f' :: a -> a
f' x = x

--b
greater (x, y) = if x > y then True else False

greater' :: Ord a => (a, a) -> Bool
greater' (x, y) = x > y

--c
h (x, y) = let z = x + y in g (z, y) where g (a, b) = a - b

h' :: Num a => (a, a) -> a
h' (x, y) = x