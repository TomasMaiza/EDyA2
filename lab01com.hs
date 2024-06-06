module Lab1C where

import Data.List
import Data.Ord

type Texto = String

{-
   Definir una función que dado un caracter y un texto calcule la frecuencia 
   con la que ocurre el caracter en el texto
   Por ejemplo: frecuency 'a' "casa" = 0.5 
-}

frecuency' :: Char -> Texto -> Float
frecuency' c [] = 0.0
frecuency' c (x:xs) | c == x = 1.0 + frecuency' c xs
                    | otherwise = frecuency' c xs

frecuency :: Char -> Texto -> Float
frecuency c [] = 0.0
frecuency c xs = frecuency' c xs / fromIntegral(toInteger (length xs))

{-                      
  Definir una función frecuencyMap que dado un texto calcule la frecuencia 
  con la que ocurre cada caracter del texto en éste.
  La lista resultado debe estar ordenada respecto a la frecuencia con la que ocurre 
  cada caracter, de menor a mayor frecuencia. 
    
  Por ejemplo: frecuencyMap "casa" = [('c',0.25),('s',0.25),('a',0.5)]

-}

unique :: [Char] -> [Char]
unique xs = [x | (x,i) <- zip xs [1..], not (elem x (take (i-1) xs))]

frecuencyMap' :: Texto -> Texto -> [(Char, Float)]
frecuencyMap' [] texto = []
frecuencyMap' (x:xs) texto = (x, frecuency x texto) : frecuencyMap' xs texto

frecuencyMap :: Texto -> [(Char, Float)]
frecuencyMap xs = (reverse . sort) (frecuencyMap' (unique xs) xs)

{-
  Definir una función subconjuntos, que dada una lista xs devuelva una lista 
  con las listas que pueden generarse con los elementos de xs.

  Por ejemplo: subconjuntos [2,3,4] = [[2,3,4],[2,3],[2,4],[2],[3,4],[3],[4],[]]
-}

{-subconjuntos' :: a -> [[a]] -> [[a]]
subconjuntos' x ls = (map (x:) ls) ++ ls

subconjuntosAux :: [a] -> [[a]]
subconjuntosAux [] = []
subconjuntosAux (x:xs)  = (subconjuntos' x [[]]) ++ subconjuntos xs

subconjuntos :: a -> [a] -> [[a]]
subconjuntos [] = []
subconjuntos (x:xs) = subconjuntosAux -}



{-
 Definir una función intercala :: a -> [a] -> [[a]]
 tal que (intercala x ys) contiene las listas que se obtienen
 intercalando x entre los elementos de ys. 
 
 Por ejemplo: intercala 1 [2,3]  ==  [[1,2,3],[2,1,3],[2,3,1]]
-}

intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys) : (map (y:) (intercala x ys)) 

{- 
  Definir una función permutaciones que dada una lista calcule todas las permutaciones
  posibles de sus elementos. Ayuda: la función anterior puede ser útil. 

  Por ejemplo: permutaciones "abc" = ["abc","bac","cba","bca","cab","acb"]
-}                  

unique' :: Eq a => [[a]] -> [[a]]
unique' xs = [x | (x,i) <- zip xs [1..], not (elem x (take (i-1) xs))]

permutaciones' :: [a] -> Int -> [[a]]
permutaciones' _ 0 = []
permutaciones' (x:xs) n = intercala x xs ++ (permutaciones' (xs ++ [x]) (n - 1))

permutaciones :: Eq a => [a] -> [[a]]
permutaciones [] = []
permutaciones xs = unique'(permutaciones' xs (length xs))