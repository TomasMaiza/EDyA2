import Seq
import Par
import ListSeq

mulMat :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
mulMat (a, b, c, d) (w, x, y, z) = (a*w + b*y, a*x + b*z, c*w + d*y, c*x + d*z)

matriz :: Int -> (Int, Int, Int, Int)
matriz _ = (1, 1, 1, 0)

--fibSeq :: Int -> [(Int, Int, Int, Int)]
--fibSeq 0 = emptyS
fibSeq n = let (s, total) = scanS mulMat (1, 0, 1, 0) (tabulate matriz n)
               s1 = appendS s (singletonS total)
           in mapS (\(a, b, c, d) -> b) s1