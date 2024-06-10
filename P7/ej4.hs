import Seq
import Par
import ListSeq

data Paren = Open | Close deriving (Show, Eq)

s1 = [Open, Open, Close, Open, Open, Close, Close, Open, Close, Close] --anda
s2 = [Open, Open, Close, Open, Close, Close] --anda
s3 = [Open, Close, Open, Open, Close, Close, Open, Open, Close] -- NO anda
s4 = [Open, Close, Open, Open, Close, Open, Close, Close, Close] -- NO anda
s5 = [Close, Open] -- NO anda
s6 = [Open, Close, Close, Open] -- NO anda


matchP :: Seq s => s Paren -> (Int, Int)
matchP s = reduceS combine val (mapS base s)
            where
              val = (0, 0)

              base Open = (0, 1)
              base Close = (1, 0)
              
              combine (a, b) (x, y) = let c = max 0 (b - 1)
                                          d = 
                                      in

--matchParen :: Seq s => s Paren -> Bool
--matchParen s = matchP s == 0

-- b
{-
matchParen :: Seq s => s Paren -> Bool
matchParen s = let (sec, tot) = scanS contar (0, 0) s
                   contar (a, b) Open = (a - 1, b + 1)
                   contar (a, b) Close = (a + 1, b - 1)
               in tot == (0, 0)
            -}