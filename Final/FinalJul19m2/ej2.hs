module Ej2 where
import Seq
import ListSeq
import Par

type Pesos = Int
type Hora = String

--alarma 〈(22, "8:00"), (30, "10:00"), (20, "10:40"), (33, "11"), (40, "11:20")〉 100 = Just "11"
s1 = [(22, "8:00"), (30, "10:00"), (20, "10:40"), (33, "11"), (40, "11:20")] :: [(Pesos, Hora)]
s2 = [(22, "8:00"), (30, "10:00")] :: [(Pesos, Hora)]
s3 = [(22, "8:00"), (30, "10:00"), (20, "10:40")] :: [(Pesos, Hora)]

{-alarma :: Seq s => s (Pesos, Hora) -> Pesos -> Maybe Hora
alarma s p = let (valor, s') = nthS s 0 ||| dropS s 1
                 combine (x, h) (y, h') = (x + y, h')
                 (sec, tot) = scanS combine valor s'
                 secu = appendS sec (singletonS tot)
             in if fst tot < p then Nothing else buscarPrecio secu p
              where
                buscarPrecio xs p | lengthS xs == 0 = Nothing
                                  | otherwise = let (pesos, hora) = nthS xs 0
                                                    resto = dropS xs 1 
                                                in if pesos >= p then Just hora else buscarPrecio resto p
-}


alarma :: Seq s => s (Pesos, Hora) -> Pesos -> Maybe Hora
alarma s p = let makeT (pesos, hora) = if pesos >= p then (pesos, hora, True)
                                                     else (pesos, hora, False)
                 s'' = mapS makeT s
                 (valor, s') = nthS s'' 0 ||| dropS s'' 1 
                 combine (x, h, flag) (y, h', flag') = let ps = x + y
                                                       in (ps, h', flag || flag' || ps >= p)
                 (secu, tot) = scanS combine valor s'
                 sec = appendS secu (singletonS tot)
                 filt = filterS (\(_, h, b) -> b) sec
             in if lengthS filt == 0 then Nothing else (\(_, h, _) -> Just h) (nthS filt 0)
