import Seq
import Par
import ListSeq

s1 :: [Int]
s1 = [12, 4, 6, 3, 2] --7

s2 :: [Int]
s2 = [4, 6, 2] --2

s3 :: [Int]
s3 = [1..5] --0

reverseS :: Seq s => s a -> s a
reverseS s | lengthS s == 0 = s
           | otherwise = appendS (reverseS (dropS s 1)) (takeS s 1)

zipS :: Seq s => s a -> s b -> s (a, b)
zipS s s' | lengthS s == 0 = emptyS
          | lengthS s' == 0 = emptyS
          | otherwise = let f1 = nthS s 0
                            f2 = nthS s' 0
                            s1 = dropS s 1
                            s2 = dropS s' 1
                            tupla = singletonS (f1, f2)
                        in appendS tupla (zipS s1 s2)


cantMultiplos :: Seq s => s Int -> Int
cantMultiplos s = let tabulado = tabulateS secuenciar (lengthS s)
                      sufijos = reverseS tabulado
                      zipeado = zipS s sufijos
                      s' = mapS multiplos zipeado
                      mult = reduceS (+) 0 s'
                  in mult
                    where
                      secuenciar n = dropS s ((lengthS s) - n)
                    
                      multiplos (i, sec) | lengthS sec == 0 = 0
                                         | otherwise = reduceS (+) 0 (mapS (modulo i) sec)
                      
                      modulo i j = if mod i j == 0 then 1 else 0
                              

--tabulateS secuenciar (lengthS s)
--secuenciar n = takeS s n