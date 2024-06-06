module Ej12P2 where
import Data.List
import Data.Char

type NumBin = [Bool]

--a
sumaBinaria :: NumBin -> NumBin -> NumBin
sumaBinaria [] ys = ys
sumaBinaria xs [] = xs
sumaBinaria (x:xs) (y:ys) | x && y = False : sumaBinaria [True] (sumaBinaria xs ys)
                          | otherwise = (x || y) : sumaBinaria xs ys

--b
productoBinarioAux :: NumBin -> Bool -> NumBin
productoBinarioAux [] _ = []
productoBinarioAux xs True = xs
productoBinarioAux xs False = [False | i <- [1..length xs]]

productoBinario :: NumBin -> NumBin -> NumBin
productoBinario [] ys = []
productoBinario xs [] = []
productoBinario xs (y:ys) = sumaBinaria (productoBinarioAux xs y) (False : productoBinario xs ys)

--c
cocienteRestoBinarioPorDos :: NumBin -> (NumBin, NumBin)
cocienteRestoBinarioPorDos [] = ([], [])
cocienteRestoBinarioPorDos (x:xs) =  (xs, [x])