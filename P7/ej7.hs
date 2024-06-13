import Seq
import Par
import ListSeq

s1 :: [Int]
s1 = [1, 4, 5, 11]

s2 :: [Int]
s2 = [0, 2, 7, 15]

data Nat = Zero | Succ Nat deriving Show

-- rel = compare

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

--d
maxS :: Seq s => (a -> a -> Ordering) -> s a -> Nat -- Nat
maxS rel s = let s' = tuplar s Zero
                 (_, (maximo, maxIndice)) = scanS max_rel (nthS s' 0) (dropS s' 1)
             in maxIndice
              where
                tuplar :: Seq s => s a -> Nat -> s (a, Nat)
                tuplar sec i | lengthS sec == 0 = emptyS
                             | otherwise = appendS (singletonS (nthS sec 0, i)) (tuplar (dropS sec 1) (Succ i))

                max_rel (a, i) (b, j) = if rel a b == GT then (a, i) else (b, j)

--e
s3 :: [Int]
s3 = [1, 1, 2, 3, 4, 4, 2, 2]

saltear :: Seq s => (a -> a -> Ordering) -> s a -> s a -> s a
saltear rel a b | lengthS a == 0 = b
                | lengthS b == 0 = a
                | otherwise = let x = nthS a 0
                                  y = nthS b 0
                              in if rel x y == EQ then a else appendS a b

group :: Seq s => (a -> a -> Ordering) -> s a -> s a
group rel s = let s' = mapS singletonS s
                  (sec, tot) = scanS (saltear rel) emptyS s'
              in tot

--f
s4 = [(2, "A"), (1, "B"), (1, "C"), (2, "D")]

juntar :: (Seq s, Ord a) => s (a, s b) -> s (a, s b) -> s (a, s b)
juntar s s' | lengthS s == 0 = s'
            | lengthS s' == 0 = s
            | otherwise = let (x, xs) = nthS s 0
                              (y, ys) = nthS s' 0
                          in if x == y then singletonS (x, appendS xs ys) else appendS s s'

--collect :: (Seq s, Ord a) => s (a, b) -> s (a, s b)-- s (a, s b)
collect s = let s' = mapS sing s
                (_, sec) = scanS juntar emptyS (sort compare s')
            in sec
              where
                sing (a, b) = singletonS (a, singletonS b)
