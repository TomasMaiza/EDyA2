import Seq
import Par
import ListSeq

s1 :: [Int]
s1 = [2, 3, 4, 7, 5, 2, 3, 2, 6, 4, 3, 5, 2, 1]

reverseS :: Seq s => s a -> s a
reverseS s = if lengthS s == 0 then emptyS else appendS (reverseS (dropS s 1)) (takeS s 1)

area :: Seq s => s Int -> s Int -> s Int -> s Int
area s maxL maxR = if lengthS s == 0 then emptyS 
                                     else let num = min (nthS maxL 0) (nthS maxR 0) - (nthS s 0)
                                              s' = (dropS s 1) 
                                              (l, r) = (dropS maxL 1) ||| (dropS maxR 1)
                                          in appendS (singletonS num) (area s' l r)

aguaHist :: Seq s => s Int -> Int
aguaHist s = let ((sL, totL), (sR, totR)) = scanS max 0 s ||| scanS max 0 (reverseS s)
                 maxL = appendS (dropS sL 1) (singletonS totL)
                 maxR = reverseS (appendS (dropS sR 1) (singletonS totR))
                 sec = area s maxL maxR
             in reduceS (+) 0 sec