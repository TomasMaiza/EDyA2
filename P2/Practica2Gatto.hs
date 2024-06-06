module Practica2 where
import Data.List
import Data.Char

{-
1-
a)
test:: (Int -> Int) -> Int ->Int

b)
esMenor:: Int -> Int -> Bool

c)
eq:: a -> b -> Bool

d)
showVal:: a -> String
-}

{-
2-
a)
(+5):: Int -> Int
Proposito: Suma 5 a un numero

b)
(0<):: Int -> Bool
Proposito: Determina si un numero es menor que 0

c)
('a':):: [Char] -> [Char]
Proposito: Agraga la letra a al inicio de una lista de caracteres

d)
(++ "\n"):: [[Char]] -> [[Char]]
Proposito: Agrega la lista "\n" al final de una lista de caracteres

e)
filter (== 7):: [Int] -> [Int]
Proposito: Filtra los elementos de una lista si son iguales a 7

f)
map (++[1]):: [[Int]] -> [[Int]]
Proposito: Recibe una lista de numeros y agrega al final de la misma el elemento 1
-}

--3)
--a)
evaluarEn1:: (Int -> Int) -> Int
evaluarEn1 f = f 1

evaluarEn5:: (Int -> Int) -> Int
evaluarEn5 f = f 5

--b)
sumar:: Int -> (Int -> Int)
sumar x y = x + y

multiplicar:: Int -> (Int -> Int)
multiplicar x y = x * y

--c)
identidad:: (Int -> Int) -> (Int -> Int)
identidad f = f

composicionPropia::(Int -> Int) -> (Int -> Int)
composicionPropia f = f . f

--d)
esCero:: Int -> Bool
esCero x = x == 0

esCinco:: Int -> Bool
esCinco x = x == 5

--e)
xor:: Bool -> (Bool -> Bool)
xor x y | x == y = False
        | otherwise = True

and:: Bool -> (Bool -> Bool)
and x y | x == y = True
        | otherwise = False

--f)
mismo:: (Int, Char) -> Bool
mismo (x, c) = ord(c) == x

mayorComp:: (Int, Char) -> Bool
mayorComp (x, c) = ord(c) > x

--g)
sumarComponentes:: (Int, Int) -> Int
sumarComponentes (x, y) = x + y

multiplicarComponentes:: (Int, Int) -> Int
multiplicarComponentes (x, y) = x * y

--h)
dobleComponente:: Int -> (Int, Int) 
dobleComponente x = (x, 2*x)

duplicarComponentes:: Int -> (Int, Int) 
duplicarComponentes x = (x, x)

--i)
f1:: a -> a
f1 x = x

-- ??

--j)
ident:: a -> a
ident x = x

aplicoFuncion:: a -> a
aplicoFuncion x = f1 x

--4
--a) 
--if true then false else true where false = True; true = False
--Esta bien

--b) if if then then else else
-- Esta mal, es un error sintactico. El segundo if debe ir despues del primer else

--c) False ≡ (5 > 4)
--Esta bien, devuelve False

--d) 1 < 2 < 3
--Esta mal, es un error de tipos, al hacer la primer comparacion se obtiene un bool, y luego se compara este bool co un int (esto esta mal)

--e) 1 + if (’a’ < ’z’) then − 1 else 0
--Esta bien, devuelve 0

--f) if fst p then fst p else snd p where p = (True, 2)
--Está mal, es un error de tipo porque then devuelve un Bool y else un Int

--g) if fst p then fst p else snd p where p = (True, False)
--Ta bien (y)


--5)
--a) 
g x = let (y, z ) = (x , x ) 
      in y

gV2 x = x
--b) 
greater (x , y) = if x > y then True else False

greaterV2 (x, y) | x > y = True
                 | otherwise = False

--c) 
f (x , y) = let z = x + y 
            in g (z , y) 
               where g (a, b) = a - b

fV2 (x, y) = (x + y) - y

--6)
--a)
smallest :: (Ord a) => a -> a -> a -> a
smallest = \x -> (\y -> (\z -> if (x <= y && x <= z) then x else if (y <= x && y <= z) then y else z))

--b)
second = \x -> x

--c)
andThen = \b -> (\y -> if b then y else b)

--d)
twice = \f -> (\x -> (f . f) x)

--e)
inc = \x -> x + 1

--7)
--a)
iff:: Bool -> Bool -> Bool
iff x y | x = not(y)
        | otherwise = y

--b)
alpha:: a -> a
alpha x = x

--8)
{-
f:: c -> d
g:: a -> b -> c

h x y = f (g x y)

Tipo de h:
h:: a -> b -> d

????
-}


--9)
zip_3:: [a] -> [b] -> [c] -> [(a, b, c)]
zip_3 (x:xs) (y:ys) (z:zs) = (x, y, z) : zip_3 xs ys zs
zip_3 _ _ _ = []

-- ?????


--10)
--a)
--Esta mal, deberia se:  [[]] ++ xs = [[], xs]

--b) 
--Esta mal, deberia se:  [[]] ++ xs = [[], xs]

--c)
--Esta mal, deberia se:  [[]] ++ xs = [[], xs]

--d)
--Esta bien

--e) [[]] ++ [xs] = [[], xs ]
--Esta mal, deberia se:  [[]] ++ [xs] = [[], [xs]]

--f)
--Esta mal, deberia se:  [[]] ++ [xs] = [[], [xs]]

--g)
--Esta mal, deberia se:  [[]] ++ [xs] = xs

--h)
--Esta bien

--i)
--Esta bien

--j)
--Esta bien 

--11)
--a)
--modulus:: [Float] -> Float

--b)
--vmod:: [[Float]] -> [Float]

--12)
type NumBin = [Bool]

--a)
sumaBinaria:: NumBin -> NumBin -> NumBin
sumaBinaria [] x = x
sumaBinaria x [] = x
sumaBinaria (x:xs) (y:ys) | x == False && y == False = x: sumaBinaria xs ys
                          | (x == False && y == True) || (y == False && x == True) = True : sumaBinaria xs ys
                          | otherwise = False: sumaBinaria (sumaBinaria xs [True]) ys


--b)
productoBinarioAux:: NumBin -> Bool -> NumBin
productoBinarioAux [] _ = []
productoBinarioAux (x:xs) y | y == False = False : productoBinarioAux xs y
                            | otherwise = x: productoBinarioAux xs y

productoBinario:: NumBin -> NumBin -> NumBin
productoBinario [] ys = []
productoBinario xs [] = []
productoBinario xs (y:ys) = sumaBinaria (productoBinarioAux xs y) (False:(productoBinario xs ys))

--c)
mayor:: NumBin -> NumBin -> Bool
mayor [] xs = False
mayor xs [] = True
mayor xs ys | length xs < length ys = False
            | length ys < length xs = True
            | (head . reverse) xs < (head . reverse) ys = False
            | (head . reverse) xs > (head . reverse) ys = True
            | (head . reverse) xs == (head . reverse) ys = mayor (reverse(tail (reverse xs))) (reverse(tail (reverse ys)))



cocienteRestoBinarioPorDos :: NumBin -> (NumBin, NumBin)
cocienteRestoBinarioPorDos [] = ([], [])
cocienteRestoBinarioPorDos (x:xs) =  (xs, [x]) 

--13)
--a)
divisores:: Int -> [Int]
divisores x = [i | i <- [1..x], mod x i == 0]

--b)
matches:: [Int] -> Int -> [Int]
matches xs y = [x | x <- xs, x /= y]

--c)
cuadruplas:: Int -> [(Int, Int, Int, Int)]
cuadruplas n = [(a,b,c,d) | a <- [1..n], b <- [1..n], c <- [1..n], d <- [1..n], a^2 + b^2 == c^2 + d^2]

--d)
unique:: [Int] -> [Int]
unique xs = [x | (x,i) <- zip xs [1..], not (elem x (take (i-1) xs))]

--14)
scalarproduct:: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]
