import Data.List
--                                                                             --
--                                   EJERCICIO 1                               --
--                                                                             --
data Tree key val = E | J Int (Tree key val) (key, val) (Tree key val)
     deriving Show

splitMax :: Tree k v -> ((k,v), Tree k v)
splitMax (J 0 l x r) = (x, E)
splitMax (J m l x E) = (x, l)
splitMax (J m l x r) = let (max, t) = splitMax r
                       in (max, J (m - 1) l x t)

delete :: (Ord k) => k -> Tree k v -> Tree k v
delete k t = if exist_key k t then delete' k t else t

delete' :: (Ord k) => k -> Tree k v -> Tree k v
delete' k (J m l (k', v) r) | k > k'    = (J (m - 1) l (k', v) (delete' k r))
                            | k < k'    = (J (m - 1) (delete' k l) (k', v) r)
                            | otherwise = let (max, t) = splitMax l
                                          in (J (m - 1) t max r)
                                           
exist_key :: (Ord k) => k -> Tree k v -> Bool
exist_key _ E                 = False
exist_key k (J m l (k', v) r) | k > k'  = exist_key k r
                              | k < k'  = exist_key k l
                              | k == k' = True

tree1 = (J 3 (J 0 E (3,3) E) (4, 4) (J 1 (J 0 E (5,5) E) (6, 6) E))
{-
       (4,4)
      /     \
    (3,3)  (6,6)
           /
         (5,5)
-}

--                                                                             --
--                                   EJERCICIO 2                               --
--                                                                             --

data Result = All Int | Some (Int, Int, Int) deriving Show

exclamation :: [Char] -> Int 
exclamation s = case (mapReduce f combine e s) of 
                  All n        -> n
                  Some (i,j,k) -> maximum [i, j, k]

e = All 0

f :: Char -> Result
f '!' = All 1
f  _  = Some(0,0,0)

combine :: Result -> Result -> Result
combine (All a) (All b)               = All (a + b)
combine (All a) (Some (i,j,k))        = Some (a + i, j, k)
combine (Some (i,j,k)) (All a)        = Some (i, j + a, k)
combine (Some (a,b,c)) (Some (i,j,k)) = Some (a, j, maximum [b + i, c, k])



exclamationsOks :: [Char] -> Bool 
exclamationsOks s = let (p, t) = scan (+) 0 (map f' s)
                    in t == 0 && length(filter (<0) p) == 0

f' :: Char -> Int 
f' '¡' = 1
f' '!' = -1
f'  _  = 0


--Auxiliares 

mapReduce :: (a -> b) -> (b -> b -> b) -> b -> [a] -> b
mapReduce m r n l = foldl r n (map m l)

scan :: (a -> a -> a) -> a -> [a] -> ([a], a)
scan f n l = let xs = scanl f n l
                 ll = length xs
             in (take (ll - 1) xs, (drop (ll - 1) xs) !! 0)

