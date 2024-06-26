import Par
import Seq
import ListSeq

-- W(n,m) = n+m, S(n,m) = O(lg n * lg m)
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

--c
maxE :: Seq s => (a -> a -> Ordering) -> s a -> a
maxE rel s = let (_, maximo) = scanS max_rel (nthS s 0) (dropS s 1)
             in maximo
              where
                max_rel a b = if rel a b == GT then a else b


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

s1 = [1, 1, 2, 3, 4, 2, 5, 2, 6, 6] :: [Int]

index :: Seq s => s a -> s (a, Int)
index s = tabulateS (\n -> (nthS s n, n)) (lengthS s)

uniquify :: Seq s => s Int -> s Int
uniquify s = let apv = index s
                 cl = collect apv
                 mapeado = mapS (\(x, xs) -> (nthS xs 0, x)) cl
                 sorted = sort ord mapeado
              in mapS snd sorted
                where
                  ord (a,b) (c,d) = compare a c

--b
s2 = "acdeaafaaa"

makeTupla :: (Char, Char) -> Int
makeTupla (c, d) = if c == 'a' && d == 'a' then 1 else 0

combine :: (Int, Int) -> (Int, Int) -> (Int, Int)
combine (x, m) (0, n) = (0, max m $ max x n)
combine (x, m) (y, n) = (x + y, max m $ max n (x + y))

aa :: Seq s => s Char -> Int
aa s = let pares = tabulateS (\n -> (nthS s n, nthS s (n + 1))) (lengthS s - 1) 
           sec = mapS makeTupla pares
           m = reduceS (+) 0 sec
       in m


