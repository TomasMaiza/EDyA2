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
sccml s = let (_, _, _, n) = reduceS combine val (mapS base s)
          in n
            where
              val = (0, 0, 0, 0)

              base v = (v, v, 0, 0)

              combine v@(prim, ult, cant, tot) v'@(prim', ult', cant', tot') = if prim' > ult
                                                                               then let cantidad = cant + cant' + 1
                                                                                        total = max cantidad $ max tot tot'
                                                                                    in (prim, ult', cantidad, total)
                                                                               else (if cant > cant'
                                                                                     then v
                                                                                     else v')

--b
--sccml' :: Seq s => s Int -> Int
sccml' s = let (sec, tot) = scan combine val (mapS base s)
               s' = (appendS sec (singletonS tot))
               (_, res) = scan max 0 (mapS snd s')
           in res
            where
              val = (0, 0)

              base v = (v, 0)

              combine x@(ult, cant) y@(ult', cant') = if ult' > ult
                                                      then (ult', cant' + cant + 1)
                                                      else y
