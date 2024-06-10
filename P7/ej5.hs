import Seq
import Par
import ListSeq

s1 = [9, 3, 5, 1, 3, 4, 5, 6, 8, 1] --5
s2 = [5, 6, 2, 3, 5, 1, 9] --2
s3 = [1, 4, 6, 7, 8, 11, 13, 3] --6

--a
sccml :: Seq s => s Int -> Int
sccml s = reduceS combine val (mapS base s)
            where
              val