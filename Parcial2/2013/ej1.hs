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

--c
s1 = [(3, 'a'), (2, 'b'), (11, 'd'), (15, 'c'), (2, 'e'), (7, 'j'), (4, 'k'), (3, 'n')]
s2 = [(1, 'z'), (2, 'j'), (13, 'f'), (16, 'h'), (7, 'n')]

data MHeap a = E | N (MHeap a) a (MHeap a) deriving Show

fromSeq :: (Ord k, Seq s) => s (k, v) -> MHeap (k, v)
fromSeq s = let c = collect s
                s' = mapS (\(x, sec) -> (x, nthS sec 0)) c
            in makeTree s'
              where
                makeTree s' | lengthS s' == 0 = E
                            | otherwise = let len = lengthS s' - 1
                                              mitad = div len 2 + 1
                                              t1 = dropS (takeS s' mitad) 1
                                              t2 = dropS s' mitad
                                              (l', r') = makeTree t1 ||| makeTree t2
                                          in N l' (nthS s' 0) r'

--dyc s = reduce combine val (mapS base s)

unir :: Ord k => MHeap (k, v) -> MHeap (k, v) -> MHeap (k, v)
unir E t = t
unir t E = t
unir t1@(N l1 (k1, v1) r1) t2@(N l2 (k2, v2) r2) | k1 == k2 = let (l, r) = unir l1 l2 ||| unir r1 r2
                                                              in N l (k1, v1) r
                                                 | k1 > k2 = let r = unir t1 r2
                                                             in N l2 (k2, v2) r
                                                 | otherwise = let r = unir r1 t2
                                                               in N l1 (k1, v1) r