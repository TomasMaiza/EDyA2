module Ej5 where
import Seq
import ListSeq
import Par

--a
sccml :: Seq s => s Int -> Int
sccml s = let base v = (v, v, 0, 0) -- valInicio / valFin / cant / tot
              val = (0, 0, 0, 0)
              combine v1@(x1, y1, c1, t1) v2@(x2, y2, c2, t2) = if y1 < x2 then let c = c1 + c2 + 1
                                                                                    t = max c $ max t1 t2
                                                                                in (x1, y2, c, t)
                                                                   else (if t1 > t2 then v1 else v2) 
              (_, _, _, n) = reduceS combine val (mapS base s) --dyc con reduce
          in n

--b
sccml' :: Seq s => s Int -> Int
sccml' s = let base v = (v, v, 0)
               val = (0, 0, 0)
               combine v1@(x1, y1, c1) v2@(x2, y2, c2) = if y1 < x2 then (x1, y2, c1 + c2 + 1) else (if c1 > c2 then v1
                                                                                               else v2)
               (sec, tot) = scanS combine val (mapS base s)
               s' = appendS sec (singletonS tot)
               n = reduceS max 0 (mapS (\(_, _, x) -> x) s')
           in n