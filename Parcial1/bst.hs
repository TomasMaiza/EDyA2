data Bin a = Hoja | Nodo (Bin a) a (Bin a) deriving Show

--member
member :: Ord a => a -> Bin a -> Bool
member x Hoja = False
member x (Nodo l a r) | x == a = True
                      | x < a = member x l
                      | x > a = member x r

--inorder
inorder :: Ord a => Bin a -> [a]
inorder Hoja = []
inorder (Nodo l a r) = (inorder l) ++ [a] ++ (inorder r)

--minimum'
minimum' :: Ord a => Bin a -> a
minimum' (Nodo Hoja a _) = a
minimum' (Nodo l a _) = minimum' l

--insert
insert :: Ord a => a -> Bin a -> Bin a
insert x Hoja = Nodo Hoja x Hoja
insert x (Nodo l a r) | x <= a = Nodo (insert x l) a r
                      | otherwise = Nodo l a (insert x r)

--delete
delete :: Ord a => a -> Bin a -> Bin a
delete x Hoja = Hoja
delete x (Nodo l a r) | x < a = Nodo (delete x l) a r
                      | x > a = Nodo l a (delete x r)
                      | otherwise = del x (Nodo l a r) where
                                                        del x (Nodo l a Hoja) = l
                                                        del x (Nodo Hoja a r) = r
                                                        del x (Nodo l a r) = let y = minimum' r
                                                                             in Nodo l y (delete y r)


bst = Nodo (Nodo Hoja 0 Hoja) 3 (Nodo (Nodo Hoja 5 (Nodo Hoja 7 Hoja)) 8 Hoja)

bst2 = Nodo (Nodo (Nodo Hoja 2 Hoja) 3 (Nodo Hoja 4 Hoja)) 9 (Nodo Hoja 11 (Nodo Hoja 15 Hoja))