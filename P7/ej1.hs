import Seq
import Par
import ListSeq

s1 :: [Int]
s1 = [2, 4, 3, 7, 9]

s2 :: [Int]
s2 = [1, 2, 5, 3, 5, 2, 7, 9]

--a 
{-}
dividir :: Seq s => s Int -> Int -> s Float
dividir s i = if lengthS s == 0 then emptyS else let division = (nthS s 0) / i
                                                   sing = singletonS division
                                                   recursion = dividir (dropS s 1) (i + 1)
                                                  in appendS sing recursion-}

tuplar :: Seq s => s Int -> Int -> s (Int, Int)
tuplar s i = if lengthS s == 0 then emptyS else let tupla = (nthS s 0, i)
                                                    sing = singletonS tupla
                                                    recursion = tuplar (dropS s 1) (i + 1)
                                                in appendS sing recursion

promedios :: Seq s => s Int -> s Int
promedios s = let (sec, tot) = scanS (+) 0 s
                  sec1 = appendS sec (singletonS tot)
                  sec2 = dropS sec1 1
                  sec3 = tuplar sec2 1
                  sec4 = mapS (\(x, y) -> div x y) sec3
              in sec4

--b
suma s = let (sec, tot) = scanS (+) 0 s
             sec1 = appendS sec (singletonS tot)
             sec2 = dropS sec1 1
         in sec2

mayores :: Seq s => s Int -> Int
mayores = undefined