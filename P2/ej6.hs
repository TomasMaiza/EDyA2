module Ej6P2 where
import Data.List
import Data.Char

--a
{-smallest :: Ord a => a -> a -> a -> a -> a
smallest = \x -> (\y -> (\z -> if (x <= y && x <= z) then x else (if (y <= x && y <= z) then y else z)))
-}
--c
andThen :: Bool -> Bool -> Bool
andThen = \x -> (\y -> if x then y else x)

--d
twice :: (a -> a) -> a -> a
twice = \f -> (\x -> (f . f) x)

--e
flip :: (a -> a -> a) -> a -> a -> a
flip = \f -> (\x -> (\y -> f y x))

--f
inc :: Num a => a -> a
inc = \x -> x + 1