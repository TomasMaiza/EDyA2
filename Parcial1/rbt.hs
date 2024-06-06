data Color = R | B deriving Show
data RBT a = E | T Color (RBT a) a (RBT a) deriving Show

--INV1: ningún nodo rojo tiene hijos rojos
--INV2: todos los caminos de la raíz a alguna hoja tienen el mismo número de nodos negros (altura negra)
--Es un árbol balanceado

--member
member :: Ord a => a -> RBT a -> Bool
member _ E = False
member x (T _ l a r) | x == a = True
                     | x < a = member x l
                     | otherwise = member x r

--insert
insert :: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack (ins x t) where
                                  ins x E = T R E x E
                                  ins x (T c l y r) | x < y = balance c (ins x l) y r
                                                    | x > y = balance c l y (ins x r)
                                                    | otherwise = T c l y r
                                  makeBlack E = E
                                  makeBlack (T _ l x r) = T B l x r

balance :: Ord a => Color -> RBT a -> a -> RBT a -> RBT a
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r

rbt = T B (T B (T R E 1 E) 2 E) 3 (T R (T B (T R E 4 E) 5 E) 7 (T B E 8 E))