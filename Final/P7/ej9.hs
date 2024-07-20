module Ej9 where
import Seq
import ListSeq
import Par

merge :: Seq s => (a->a->Ordering) -> s a -> s a -> s a
merge ord xs ys
        | lengthS xs == 0 = ys
        | otherwise = let
                        -- O(1)
                        pivot = nthS xs (lengthS xs `div` 2)

                        -- Para cada uno de estos, W = O(n), S = O(lg n)
                        (xless,yless) = filterS ((==GT) . ord pivot) xs ||| filterS ((==GT) . ord pivot) ys
                        (xgreat,ygreat) = filterS ((==LT) . ord pivot) xs ||| filterS ((==LT) . ord pivot) ys
                        (xeq,yeq) = filterS ((==EQ) . ord pivot) xs ||| filterS ((==EQ) . ord pivot) ys
                      in
                        -- W = O(n+m), S=O(1)
                        merge ord xless yless `appendS` xeq `appendS` yeq `appendS` merge ord xgreat ygreat


sort :: Seq s => (a -> a -> Ordering) -> s a -> s a
sort comp s = case lengthS s of
                0 -> s
                _ -> let s' = mapS singletonS s
                         (val, sec) = (nthS s' 0, dropS s' 1)
                     in reduceS (merge comp) val sec

collect s = let ordenar xs = sort (\(a, _) (c, _) -> compare a c) xs
                comparar_claves s1 s2 | lengthS s1 == 0 = s2
                                      | lengthS s2 == 0 = s1
                                      | otherwise = let len1 = lengthS s1
                                                        (c, v1) = nthS s1 (len1 - 1) --ult
                                                        (k, v2) = nthS s2 0 --prim
                                                    in if c == k then let p = takeS s1 (len1 - 1)
                                                                          p' = dropS s2 1
                                                                      in  appendS p $ appendS (singletonS (c, appendS v1 v2)) p'
                                                                 else appendS s1 s2 
                juntar xs = let (_, fin) = scanS comparar_claves emptyS xs in fin
                s' = ordenar s
                sec = mapS (\(a, b) -> singleton (a, singleton b)) s'
            in juntar sec

--mapCollectReduce :: (Ord a, Seq s) => (s a -> s (s b)) -> ()
mapCollectReduce apv red s = let pairs = joinS (mapS apv s)
                                 groups = collect pairs
                             in mapS red groups

--a
s1 = ["hola",
      "como",
      "andas",
      "bien",
      "y",
      "vos",
      "mal",
      "ok",
      "chau"]

--countCaract :: Seq s => s (s Char) -> s (Char, Int)
countCaract s = mapCollectReduce apv red s
                  where
                    --apv :: Seq s =>  s (s Char) -> s (s (Char, Int))
                    apv xs = map (\c -> (c, 1)) xs
                    red :: Seq s => (Char, s Int) -> (Char, Int)
                    red (c, xs) = (c, lengthS xs) 

--b
--huffman :: Seq s => s (s Char) -> s (Int, s Char)
huffman s = let s' = countCaract s
                sec = mapS (\(x, y) -> (y, x)) s'
            in collect sec