module Ej2 where
import Seq
import ListSeq
import Par

--maxBalance〈−2, −4, 6, −1, 5, −7, 2〉= 10

s1 = [(-2), (-4), 6, (-1), 5, (-7), 2] :: [Int]
s2 = [2, -1, 3] :: [Int]

--combine :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
combine (pref, i, f, suf) (pref', i', f', suf') = let suma = suf + pref'
                                                      suma2 = suma + suf'
                                                      suma3 = pref + suma2
                                                      m = max suma $ max suma2 $ max suma3 $ max suf pref'
                                                  in case m of
                                                    suma -> (pref, f, i', suma)
                                                    suma2 -> (suf, f, f', suma2)
                                                    suma3 -> (pref, i, f', suma3)
                                                    suf -> (pref, i, f, suf)
                                                    pref' -> (pref', i', f', suf')

                                              

--maxBalance :: Seq s => s Int -> Int
maxBalance s = let s' = mapS (\x -> (x, x, x, x)) s -- inicio, final, suma
                   (sec, val) = dropS s' 1 ||| nthS s' 0
                   res = reduceS combine val sec
                   (_, _, _, m) = res
               in res
