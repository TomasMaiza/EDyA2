module Ej9P2 where
import Data.List
import Data.Char

zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3' xs ys zs

zipaux :: [((a, b), c)] -> [(a, b, c)]
zipaux [] = []
zipaux (((x, y), z):xs) = (x, y, z) : zipaux xs

zip'' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip'' xs ys zs = zipaux (zip (zip xs ys) zs)