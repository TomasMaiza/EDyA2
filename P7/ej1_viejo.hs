-- a
promedios : Seq Int -> Seq Float
promedios seq = if length seq === 0 then seq
                                    else dividir (drop 1 (append fst(scan (+) 0 seq) snd(scan (+) 0 seq)) ) 1

dividir : Seq Int -> Seq Float
dividir seq n = if length seq === 0 then seq 
                                    else append ((take 1 seq) / n) (dividir (drop 1 seq) (n + 1))


-- b
mayores : Seq Int → Int
mayores seq = if length seq === 0 then seq
                                  else filtrar (drop 1 (append fst(scan (+) 0 seq) snd(scan (+) 0 seq))) seq 0


filtrar : Seq Int -> Int
filtrar seqMOD seqOG n = if length seqOG === n then 0
                                               else (if (nth seqMOD n) - 2*(nth seqOG) < 0 then 1 + seqMOD seqOG (n+1)
                                                                                            else seqMOD seqOG (n+1))