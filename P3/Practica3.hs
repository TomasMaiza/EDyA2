module Practica3 where
import Data.List
import Data.Char

-- Ej 1
type Red = Float
type Green = Float
type Blue = Float
data Color = RGB Red Green Blue deriving Show

mezclar :: Color -> Color -> Color
mezclar (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB ((r1 + r2)/2.0) ((g1 + g2)/2.0) ((b1 + b2)/2.0)

-- Ej 2
type Linea = (Int, String)

punteroInvalido :: Linea -> Bool
punteroInvalido (p, xs) = p > length xs || p < 0

vacia :: Linea
vacia = (0, "")

moverIzq :: Linea -> Linea
moverIzq (0, "") = (0, "")
moverIzq l@(p, xs) | punteroInvalido l = error "Puntero invalido"
                   | otherwise = (p - 1, xs)

moverDer :: Linea -> Linea
moverDer (0, "") = (0, "")
moverDer l@(p, xs) | punteroInvalido l = error "Puntero invalido"
                   | p == length xs = l
                   | otherwise = (p + 1, xs)

moverIni :: Linea -> Linea
moverIni (p, xs) = (0, xs)

moverFin :: Linea -> Linea
moverFin (p, xs) = (length xs, xs)

insertar :: Char -> Linea -> Linea
insertar c (p, xs) = (p + 1, take p xs ++ [c] ++ drop p xs)

borrar :: Linea -> Linea
borrar (0, xs) = (0, xs)
borrar l@(p, xs) | punteroInvalido l = error "Puntero invalido"
                 | otherwise = (p - 1, take (p - 1) xs ++ drop p xs)

-- Ej 3
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

{-
[] --> EmptyCL
[1] --> CUnit 1
[1, 2] --> Consnoc 1 EmptyCL 2
[1, 2, 3] --> Consnoc 1 (CUnit 2) 3
[1, 2, 3, 4, 5] --> Consnoc 1 (Consnoc 2 (CUnit 3) 4) 5
-}

--a

headCL :: CList a -> a
headCL (CUnit x) = x
headCL (Consnoc x xs y) = x

tailCL :: CList a -> CList a
tailCL (CUnit x) = EmptyCL
tailCL (Consnoc x EmptyCL y) = CUnit y
tailCL (Consnoc x xs y) = Consnoc (headCL xs) (tailCL xs) y

isEmptyCL :: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _ = False

isCUnit :: CList a -> Bool
isCUnit (CUnit x) = True
isCUnit _ = False

--b
reverseCL :: CList a -> CList a
reverseCL EmptyCL = EmptyCL
reverseCL (CUnit x) = CUnit x
reverseCL (Consnoc x xs y) = Consnoc y (reverseCL xs) x

--c

cons :: a -> CList a -> CList a --inserta un elemento al principio
cons x EmptyCL = CUnit x
cons x (CUnit y) = Consnoc x EmptyCL y
cons x (Consnoc y ys z) = Consnoc x (cons y ys) z

snoc :: CList a -> a -> CList a --inserta un elemento al final
snoc ls x = reverseCL (cons x (reverseCL ls))

append' :: CList a -> CList a -> CList a --concatena dos listas CL
append' EmptyCL ys = ys
append' xs EmptyCL = xs
append' xs ys = append' (snoc xs (headCL ys)) (tailCL ys)

mapCL :: (a -> b) -> CList a -> CList b
mapCL f EmptyCL = EmptyCL
mapCL f (CUnit x) = CUnit (f x)
mapCL f (Consnoc x xs y) = snoc (cons (f x) (mapCL f xs)) (f y)

initsCL :: CList a -> CList (CList a)
initsCL EmptyCL = CUnit (EmptyCL)
initsCL (CUnit x) = Consnoc EmptyCL EmptyCL (CUnit x)
initsCL l@(Consnoc x xs y) = Consnoc EmptyCL (mapCL (cons x) (initsCL xs)) l

--e
concatCL :: CList (CList a) -> CList a
concatCL EmptyCL = EmptyCL
concatCL (Consnoc xs xss ys) = append' xs (append' (concatCL xss) ys)

-- Ej 4
data Exp = Lit Int | Add Exp Exp | Sub Exp Exp | Prod Exp Exp | Div Exp Exp deriving Show

eval :: Exp -> Int
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Sub x y) = (eval x) - (eval y)
eval (Prod x y) = (eval x) * (eval y)
eval (Div x y) = div (eval x) (eval y)

-- Ej 5

--a
type Pila = [Exp]

buscarCaracter :: String -> Int
buscarCaracter [] = 0
buscarCaracter (x:xs) | x == ' ' = 0
                      | otherwise = 1 + buscarCaracter xs 

{-stackear :: String -> Pila -> Pila
stackear [] stack = stack
stackear xs stack = stackear (drop (p + 1) xs) ((take p xs) : stack) where p = buscarCaracter xs-}

traductorRPN :: String -> Pila -> Pila
traductorRPN [] ys = ys
traductorRPN xs [] = [Lit z] where z = read xs :: Int
traductorRPN xs [y] = Lit z : [y] where z = read xs :: Int
traductorRPN xs l@(x:y:ys) | xs == "+" = (Add y x) : ys
                         | xs == "-" = (Sub y x) : ys
                         | xs == "*" = (Prod y x) : ys
                         | xs == "/" = (Div y x) : ys
                         | otherwise = Lit z : l where z = read xs :: Int

stackearExp :: String -> Pila -> Pila
stackearExp [] stack = stack
stackearExp xs stack = stackearExp d (traductorRPN t stack) where p = buscarCaracter xs
                                                                  d = drop (p + 1) xs
                                                                  t = take p xs

parseRPN :: String -> Exp
parseRPN xs = head(stackearExp xs [])

--b
evalRPN :: String -> Int
evalRPN xs = (eval . parseRPN) xs

-- Ej 6
--a) No los maneja, salta la excepción de la función div.

--b
maybeToInt :: Maybe Int -> Int
maybeToInt (Just x) = x

seval :: Exp -> Maybe Int
seval (Lit x) = Just x
seval (Add x y) = Just (maybeToInt (seval x) + maybeToInt (seval y))
seval (Sub x y) = Just (maybeToInt (seval x) - maybeToInt (seval y))
seval (Prod x y) = Just (maybeToInt (seval x) * maybeToInt (seval y))
seval (Div x (Lit 0)) = Nothing
seval (Div x y) = Just (div (maybeToInt(seval x)) (maybeToInt(seval y)))
