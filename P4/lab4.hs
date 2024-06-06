module Lab02 where

{-
   Laboratorio 2
   EDyAII 2022
-}

import Data.List

-- 1) Dada la siguiente definición para representar árboles binarios:

data BTree a = E | Leaf a | Node (BTree a) (BTree a) deriving Show
btree = Node (Node (Leaf 8) (Leaf 3)) (Node (Node (Node (Leaf 11) (Leaf 4)) (Leaf 3)) (Leaf 12))

-- Definir las siguientes funciones:

-- a) altura, devuelve la altura de un árbol binario.

altura :: BTree a -> Int
altura E = 0
altura (Leaf _) = 1
altura (Node l r) = 1 + max (altura r) (altura l)

-- b) perfecto, determina si un árbol binario es perfecto (un árbol binario es perfecto si cada nodo tiene 0 o 2 hijos
-- y todas las hojas están a la misma distancia desde la raı́z).

perfecto :: BTree a -> Bool
perfecto E = True
perfecto (Node (Leaf _) (Leaf _)) = True
perfecto (Node (Leaf _) (Node _ _)) = False
perfecto (Node (Node _ _) (Leaf _)) = False
perfecto (Node l r) = perfecto l && perfecto r

-- c) inorder, dado un árbol binario, construye una lista con el recorrido inorder del mismo.

inorder :: BTree a -> [a]
inorder E = []
inorder (Leaf a) = [a]
inorder (Node l r) = inorder l ++ inorder r


-- 2) Dada las siguientes representaciones de árboles generales y de árboles binarios (con información en los nodos):

data GTree a = EG | NodeG a [GTree a] deriving Show
gtree = NodeG 'A' [(NodeG 'B' [(NodeG 'F' [EG]), (NodeG 'G' [EG])]), (NodeG 'C' [EG]), (NodeG 'D' [EG]), (NodeG 'E' [EG])]

data BinTree a = EB | NodeB (BinTree a) a (BinTree a) deriving Show

{- Definir una función g2bt que dado un árbol nos devuelva un árbol binario de la siguiente manera:
   la función g2bt reemplaza cada nodo n del árbol general (NodeG) por un nodo n' del árbol binario (NodeB ), donde
   el hijo izquierdo de n' representa el hijo más izquierdo de n, y el hijo derecho de n' representa al hermano derecho
   de n, si existiese (observar que de esta forma, el hijo derecho de la raı́z es siempre vacı́o).
   
   
   Por ejemplo, sea t: 
       
                    A 
                 / | | \
                B  C D  E
               /|\     / \
              F G H   I   J
             /\       |
            K  L      M    
   
   g2bt t =
         
                  A
                 / 
                B 
               / \
              F   C 
             / \   \
            K   G   D
             \   \   \
              L   H   E
                     /
                    I
                   / \
                  M   J  
-}
g2bt_aux :: GTree a -> GTree a -> BinTree a
g2bt_aux EG gt = g2bt gt
g2bt_aux (NodeG a []) gt = NodeB EB a (g2bt gt)
g2bt_aux (NodeG a [x]) gt = NodeB (g2bt_aux x EG) a (g2bt gt)
g2bt_aux (NodeG a (x:y:xs)) gt = NodeB (g2bt_aux x y) a (g2bt gt)

g2bt :: GTree a -> BinTree a
g2bt EG = EB
g2bt gt@(NodeG a (x:xs)) = g2bt_aux gt EG


-- 3) Utilizando el tipo de árboles binarios definido en el ejercicio anterior, definir las siguientes funciones: 
{-
   a) dcn, que dado un árbol devuelva la lista de los elementos que se encuentran en el nivel más profundo 
      que contenga la máxima cantidad de elementos posibles. Por ejemplo, sea t:
            1
          /   \
         2     3
          \   / \
           4 5   6
                             
      dcn t = [2, 3], ya que en el primer nivel hay un elemento, en el segundo 2 siendo este número la máxima
      cantidad de elementos posibles para este nivel y en el nivel tercer hay 3 elementos siendo la cantidad máxima 4.
   -}

bintree = NodeB (NodeB EB 2 (NodeB EB 4 EB)) 1 (NodeB (NodeB EB 5 EB) 3 (NodeB EB 6 EB))
bintree2 = NodeB (NodeB (NodeB EB 7 EB) 0 (NodeB EB 8 EB)) 3 (NodeB (NodeB EB 5 EB) 6 (NodeB EB 9 EB))

type Nivel = Int

nodo :: BinTree a -> a
nodo (NodeB _ x _) = x

hijos :: BinTree a -> [BinTree a]
hijos EB = []
hijos (NodeB EB _ r) = [r]
hijos (NodeB l _ EB) = [l]
hijos (NodeB l _ r) = [l, r]

dcn_aux :: Nivel -> [BinTree a] -> [a]
dcn_aux n xs = let nodos = 2^n
                   ls = map hijos xs
                   ys = concat ls
                   len = length ys
                   comp = len == nodos
                   niv = n + 1
                   zs = map nodo xs
               in if comp then concat(map (dcn_aux niv) ls) else zs

dcn :: BinTree a -> [a]
dcn EB = []
dcn (NodeB EB x _) = [x]
dcn (NodeB _ x EB) = [x]
dcn bt = dcn_aux 1 [bt]

{- b) maxn, que dado un árbol devuelva la profundidad del nivel completo
      más profundo. Por ejemplo, maxn t = 2   -}

maxn_aux :: Nivel -> [BinTree a] -> Nivel
maxn_aux n xs = let nodos = 2^n
                    ls = map hijos xs
                    ys = concat ls
                    len = length ys
                    comp = len == nodos
                    niv = n + 1
                in if comp then minimum(map (maxn_aux niv) ls) else n

maxn :: BinTree a -> Int
maxn EB = 0
maxn bt = maxn_aux 1 [bt]

{- c) podar, que elimine todas las ramas necesarias para transformar
      el árbol en un árbol completo con la máxima altura posible. 
      Por ejemplo,
         podar t = NodeB (NodeB EB 2 EB) 1 (NodeB EB 3 EB)
-}

podar_aux :: Nivel -> BinTree a -> BinTree a
podar_aux 0 bt = EB
podar_aux n bt@(NodeB l x r) = NodeB (podar_aux niv l) x (podar_aux niv r) where niv = n - 1 

podar :: BinTree a -> BinTree a
podar EB = EB
podar (NodeB EB x _) = NodeB EB x EB
podar (NodeB _ x EB) = NodeB EB x EB
podar bt = podar_aux nivelmax bt where nivelmax = maxn bt