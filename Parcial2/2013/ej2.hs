import Par
import Seq
import ListSeq

s1 = [(0, 5), (1, 2), (3, 5)] :: [(Int, Int)] -- 2
s2 = [(1, 5), (2, 7), (3, 4)] :: [(Int, Int)] -- 1

tupla :: Seq s => s (Int, Int) -> s ((Int, Int), s (Int, Int))
tupla s = tabulateS (\n -> (nthS s n, dropS s (n + 1))) (lengthS s)

contencion :: (Int, Int) -> (Int, Int) -> Bool
contencion (x, y) (z, w) = (z < x && y < w) || (x < z && w < y)

contar :: Seq s => ((Int, Int), s (Int, Int)) -> Bool
contar (int, sec) = let s' = mapS (contencion int) sec
                        s = reduceS (||) False s'
                    in s

count :: Seq s => s (Int, Int) -> Int
count s = let s' = tupla s
              sec = mapS contar s'
              ss = reduceS (+) 0 (mapS (\x -> if x then 1 else 0) sec)
          in ss