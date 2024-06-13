import Par
import Seq
import ListSeq

longestStreak :: Seq s => Float -> s Float -> Int
longestStreak val s = let s' = mapS (\x -> if x > val then 1 else 0) s
                          (s'', total) = scanS contar 0 s'
                          sec = appendS (dropS s'' 1) (singletonS total)
                          long = reduceS max 0 sec
                      in long
                        where
                          contar x y = if y == 0 then 0 else x + y


s1 = [20, 21, 27, 30, 24] :: [Float]
s2 = [28, 31, 32, 29, 31, 31, 33, 29] :: [Float]
s3 = [28, 31, 29, 31, 29] :: [Float]