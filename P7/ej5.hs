import Seq
import Par
import ListSeq

s1 :: [Int]
s1 = [9, 3, 5, 1, 3, 4, 5, 6, 8, 1] --5

s2 :: [Int]
s2 = [5, 6, 2, 3, 5, 1, 9] --2

s3 :: [Int]
s3 = [1, 4, 6, 7, 8, 11, 13, 3] --6

--a
sccml :: Seq s => s Int -> Int
sccml s = let (_, _, n) = reduceS combine val (mapS base s)
          in n
            where
              val = (0, 0, 0)

              base v = (v, 0, 0)

              combine (anterior, cantidad, masLargo) (v, _, _) = if v > anterior
                                                                 then let cant = cantidad + 1
                                                                          largo = max masLargo cant
                                                                      in (v, cant, largo)
                                                                 else (v, 0, masLargo)