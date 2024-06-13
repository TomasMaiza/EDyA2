import Par
import Seq
import ListSeq
-- prefijo
-- más larga que no es ni prefijo ni sufijo
-- sufijo
data Result = All Int | Some (Int, Int, Int) deriving Show

mapReduce :: Seq s => (a -> b) -> (b -> b -> b) -> b -> s a -> b
mapReduce f combine e s = reduceS combine e (mapS f s)

f :: Char -> Result
f c = if c == '!' then All 1 else All 0

combine :: Result -> Result -> Result
combine (All 0) (All x) = Some (0, x, x)
combine (All x) (All 0) = Some (x, x, x)
combine (All x) (All y) = All (x + y)
combine (All x) (Some (i, m, j)) = Some (x + i, m, j)
combine (Some (i, m, j)) (All 0) = Some (i, m, j)
combine (Some (i, m, j)) (All x) = Some (i, m, j + x)
combine (Some (i, m, j)) (Some (i', m', j')) = Some (i, max m $ max m' (j + i'), j')

e = All 0 :: Result

exclamation :: Seq s => s Char -> Int
exclamation s = case (mapReduce f combine e s) of
                  All n -> n
                  Some (i, j, m) -> max i $ max j m

s1 = "!!!!!!"
s2 = "!!ab!!!c!"
s3 = "hola!!chau!!!"

--b
excl :: Char -> Int
excl c | c == '¡' = 1
       | c == '!' = -1
       | otherwise = 0

exclamationOks :: Seq s => s Char -> Bool
exclamationOks s = let (_, n) = scanS (+) 0 (mapS excl s)
                   in n == 0

s4 = "¡Hola!!"
s5 = "¡¡Hola!!"