module Ej7P2 where
import Data.List
import Data.Char

--a
iff :: Bool -> Bool -> Bool
iff x y | x = not y
        | otherwise = y

--b
alpha :: a -> a
alpha x = x