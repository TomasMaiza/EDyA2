import Seq
import Par
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

maxE :: Seq s => (a -> a -> Ordering) -> s a -> a
maxE rel s = let (_, maximo) = scanS max_rel (nthS s 0) (dropS s 1)
             in maximo
              where
                max_rel a b = if rel a b == GT then a else b

mapCollectReduce apv red s = let pairs = joinS (mapS apv s)
                                 groups = collect pairs
                             in mapS red groups

promedio :: Seq s => s Int -> Int
promedio s = let len = lengthS s
                 suma = reduceS (+) 0 s
             in div suma len

clave :: Int -> Int
clave x | x >= 70 = 1
        | x <= 50 = 3
        | otherwise = 2

apv :: Seq s => (String, s Int) -> s (Int, Int)
apv (nombre, notas) = let prom = promedio notas
                          key = clave prom
                          maximo = maxE compare notas
                      in singletonS (key, maximo)

red :: Seq s => (Int, s Int) -> (Int, Int)
red (key, notas) = (lengthS notas, maxE compare notas)

datosIngreso :: Seq s => s (String, s Int) -> s (Int, Int)
datosIngreso s = mapCollectReduce apv red s


s1 = [("Martin", [50, 25, 15]),
      ("Federico", [70, 64, 86]),
      ("Lautaro", [74, 43, 12]),
      ("Andres", [56, 67, 61]),
      ("Santiago", [63, 72, 70]),
      ("Ramiro", [71, 71, 80]),
      ("Agustin", [13, 79, 29])] :: [(String, [Int])]