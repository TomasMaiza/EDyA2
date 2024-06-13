import Seq
import Par
import ListSeq

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

-- W(n) = n lg n , S(n) = lg^2 n
sort :: Seq s => (a->a->Ordering) -> s a -> s a
sort ord = reduceS (merge ord) emptyS . mapS singletonS

juntar :: (Seq s, Ord a) => s (a, s b) -> s (a, s b) -> s (a, s b)
juntar s s' | lengthS s == 0 = s'
            | lengthS s' == 0 = s
            | otherwise = let (x, xs) = nthS s 0
                              (y, ys) = nthS s' 0
                          in if x == y then singletonS (x, appendS xs ys) else appendS s s'

collect:: (Ord a, Seq s) => s (a,b) -> s (a, s b)
collect xs = let
                    ord (a,b) (c,d) = compare a c
                    sorted_xs = sort ord xs
             in
                    reduceS combine emptyS $ mapS (\(a,b) -> singletonS (a, singletonS b)) sorted_xs
             where
                combine xs ys 
                            | lengthS xs == 0 = ys
                            | lengthS ys == 0 = xs
                            | otherwise = let
                                            (lastXS, headYS) = nthS xs (lengthS xs -1) ||| nthS ys 0
                                          in
                                            if fst lastXS == fst headYS
                                            then takeS xs (lengthS xs - 1) `appendS` singletonS (fst lastXS, snd lastXS `appendS` snd headYS) `appendS` dropS ys 1
                                            else xs `appendS` ys

mapCollectReduce apv red s = let pairs = joinS (mapS apv s)
                                 groups = collect pairs
                             in mapS red groups

apv :: Seq s => s Char -> s (Char, Int)
apv s = mapS contarChar s
        where
          contarChar c = (c, 1)

red :: Seq s => (Char, s Int) -> (Char, Int)
red (c, cant) = (c, lengthS cant)

--a
countCaract :: Seq s => s (s Char) -> s (Char, Int)
countCaract s = mapCollectReduce apv red s

s1 = ["hola",
      "como",
      "andas",
      "bien",
      "y",
      "vos",
      "mal",
      "ok",
      "chau"]

--b
huffman :: Seq s => s (s Char) -> s (Int, s Char)
huffman s = let apariciones = countCaract s
                s' = mapS (\(c, n) -> (n, c)) apariciones
                sec = collect s'
            in sec