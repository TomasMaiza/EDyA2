fibSeq : Nat → Seq Nat
fibSeq n = if n === 0 then singleton 0
                      else fst(scan matriz_prod <1, 0, 1, 0> (tabulate matriz (n + 1)))

<0, 1, 2, 3, 4, 5, 6, 7, 8>
<0, 1, 1, 2, 3, 5, 8, 13, 21>

matriz = <1, 1, 1, 0>

matriz_prod : Seq Nat -> Seq Nat -> Nat
matriz_prod 

--
type Tup = (Int, Int, Int, Int)

mulmat :: Tup -> Tup -> Tup
mulmat (a1, b1, c1, d1) (a2, b2, c2, d2) = (a1*a2 + b1*c2, a1*b2 + b1*d2, c1*a2 + d1*c2, c1*b2 + d1*d2)

fibo :: Int -> Int
fibo (-1) = 1
fibo 0 = 0
fibo 1 = 1
fibo (n) = fibo(n-1) + fibo(n-2)

fibSeq :: Int -> Tup
fibSeq n = snd(scanL (mulmat) (1,0,0,1) (tabulateL (\x -> (1,1,1,0)) n))