module Ej7 where
import Seq
import ListSeq
import Par

s1 = [1, 4, 9, 12, 13, 15] :: [Int]
s2 = [2, 5, 7, 14, 19] :: [Int]
s3 = [0, 3, 4, 8, 13] :: [Int]

s4 = "adfhj"
s5 = "bcgi"

{-
[([1], [2, 5, 7, 14, 19]),
 ([4], [5, 7, 14, 19]),
 ([9], [14, 19]),
 ([12], [14, 19]),
 ([13], [14, 19]),
 ([15], [19])]

comb t1@([1], [2, 5, 7, 14, 19]) t2@([4], [5, 7, 14, 19]) = if 1 <= 2 && 2 <= 4 
                                                            then comb ([1, 2], [5, 7, 14, 19]) t2
                                                            else (if 1 <= 2)

if 2 <= 4 then comb ([1, 2], [5, 7, 14, 19]) t2 else ([1, 4], [5, 7, 14, 19])

if nthS may1 0 <= nthS ys 0 
then comb (appendS xs (nthS may1 0), (dropS 1 may1)) v2
else (appendS xs (nthS ys 0), may2)-}


--a
{-
merge :: Seq s => (a -> a -> Ordering) -> s a -> s a -> s a
merge comp s1 s2 = let base v = (singletonS v, filterS (\x -> comp v x /= GT) s2)
                       sec = mapS base s1
                       (val, s') = (nthS sec 0, dropS sec 1)
                       combine v1@(xs, may1) v2@(ys, may2) | lengthS may1 == 0 = (appendS xs ys, may2)
                                                           | otherwise = let m = nthS may1 0
                                                                             y = nthS ys 0
                                                                             x = nthS xs 0
                                                                         in if comp x y == GT then combine v2 v1
                                                                            else case comp m y of
                                                                                  GT -> (appendS xs ys, may2)
                                                                                  _ -> combine (appendS xs (singletonS m), (dropS may1 1)) v2
                       (secu, _) = reduceS combine val s'
                   in secu-}
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

--b

s6 = [4, 2, 7, 3, 1, 5] :: [Int]

sort :: Seq s => (a -> a -> Ordering) -> s a -> s a
sort comp s = case lengthS s of
                0 -> s
                _ -> let s' = mapS singletonS s
                         (val, sec) = (nthS s' 0, dropS s' 1)
                     in reduceS (merge comp) val sec

--c
maxE :: Seq s => (a -> a -> Ordering) -> s a -> a
maxE comp s = let (val, s') = nthS s 0 ||| dropS s 1
                  combine a b = if comp a b == GT then a else b 
                  maximo = reduceS combine val s'
              in maximo

--d
--maxS :: Seq s => (a -> a -> Ordering) -> s a -> Int
maxS comp s = let len = lengthS s
                  sec = tabulate (\i -> (nthS s i, i)) len
                  (val, s') = nthS sec 0 ||| dropS sec 1
                  combine t1@(a, i1) t2@(b, i2) = if comp a b == GT then t1 else t2 
                  (e, i) = reduceS combine val s'
              in i

--e
s7 = [1, 1, 2, 3, 4, 4, 2, 2] :: [Int]


group :: Seq s => (a -> a -> Ordering) -> s a -> s a
group comp s = let s' = mapS singletonS s
                   saltear xs ys | lengthS xs == 0 = ys
                                 | lengthS ys == 0 = xs 
                                 | otherwise = let x = nthS xs (lengthS xs - 1)
                                                   y = nthS ys 0
                                               in if comp x y == EQ then appendS xs $ dropS xs 1 else appendS xs ys
                   (_, res) = scanS saltear emptyS s'
               in res

--f
s8 = [(2, "A"), (1, "B"), (1, "C"), (2, "D")]
s9 = [(3, "EDyAII"), (1, "Prog1"), (2, "Prog2"), (2, "EDyAI"), (3, "SOI")]

--collect :: Seq s => s (a, b) -> s (a, s b)
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
{-
[[(2, ["A"])], [(1, ["B"])], [(1, ["C"])], [(2, ["D"])]]
[[(1, ["B"])], [(1, ["C"])], [(2, ["A"])], [(2, ["D"])]]-}