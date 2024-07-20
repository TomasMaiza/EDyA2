module Ej1 where
import Seq
import ListSeq
import Par

data Art = A | B | C deriving (Show, Eq)

s1 = [(A, 5), (B, 4), (A, 4), (A, 30), (B, 7), (B, 10)] :: [(Art, Int)] -- (b, 2)
s2 = [(A, 5), (A, 6), (B, 1), (A, 3), (A, 4), (B, 2)] :: [(Art, Int)] -- (a, 2)


buscar_precio :: Seq s => s (Art, Int, Int) -> s (Art, Int, Int) -> s (Art, Int, Int)
buscar_precio s e | lengthS s == 0 = e
                  | otherwise = let (y, p', c') = nthS s 0
                                    (x, p, c) = nthS e 0
                                in if x == y 
                                   then (if p > p' then singletonS (x, p, c + c' + 1) else singletonS (x, p, c + c'))
                                   else appendS (takeS s 1) (buscar_precio (dropS s 1) e)


--aumentos :: Seq s => s (Art, Int) -> (Art, Int)
aumentos s = let s' = mapS (\(x, p) -> singleton (x, p, 0)) s
                 combine xs ys | lengthS xs == 0 = ys
                               | lengthS ys == 0 = xs
                               | otherwise = buscar_precio xs (takeS ys 1)
             in scanS combine emptyS s'