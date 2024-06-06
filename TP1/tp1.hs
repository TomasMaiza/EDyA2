-- GRUPO: Ramiro Gatto, Tomás Maiza

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v) | Leaf k v | E deriving Show

--a
search :: Ord k => [k] -> TTree k v -> Maybe v
search _ E = Nothing
search (x:xs) (Leaf k v) = case xs == [] of
                                True -> if x == k then Just v else Nothing
                                False -> Nothing
search ls@(x:xs) (Node k val l m r) | x > k = search ls r
                                    | x < k = search ls l
                                    | otherwise = case xs == [] of
                                                       True -> if x == k then val else Nothing
                                                       False -> search xs m

--b
insert :: Ord k => [k] -> v -> TTree k v -> TTree k v
insert [] _ tree = tree 
insert [x] v E = Leaf x v
insert (x:xs) v E = Node x Nothing E (insert xs v E) E
insert [x] v hoja@(Leaf k val) = if x == k then Leaf k v 
                                           else (if x > k then Node k (Just val) E E (Leaf x v)  
                                                          else Node k (Just val) (Leaf x v) E E)
insert ls@(x:xs) v hoja@(Leaf k val) = if x > k then Node k (Just val) E E (insert ls v E)
                                                else (if x < k then Node k (Just val) (insert ls v E) E E else Node k (Just val) E (insert xs v E) E)
insert ls@(x:xs) v (Node k val l m r) | x > k = Node k val l m (insert ls v r)
                                      | x < k = Node k val (insert ls v l) m r
                                      | otherwise = case xs == [] of
                                                         True -> Node k (Just v) l m r
                                                         False -> Node k val l (insert xs v m) r

--c
delete :: Ord k => [k] -> TTree k v -> TTree k v
delete [] tree = tree
delete _ E = E
delete [x] hoja@(Leaf k v) = if x == k then E else hoja
delete (x:y:xs) hoja@(Leaf k v) = hoja
delete ls@(x:xs) tree@(Node k val l m r) | x > k = Node k val l m (delete ls r)
                                         | x < k = Node k val (delete ls l) m r
                                         | otherwise = case xs == [] of
                                                            True -> Node k Nothing l m r
                                                            False -> Node k val l (delete xs m) r

--d
keys_aux :: TTree k v -> [k] -> [[k]]
keys_aux E ks = []
keys_aux (Leaf k _) ks = [ks ++ [k]]
keys_aux (Node k Nothing l m r) ks = keys_aux l ks ++ keys_aux m prefijo ++ keys_aux r ks where prefijo = ks ++ [k]
keys_aux (Node k (Just v) l m r) ks = keys_aux l ks ++ (prefijo : keys_aux m prefijo) ++ keys_aux r ks where prefijo = ks ++ [k]

keys :: TTree k v -> [[k]]
keys E = []
keys (Leaf k _) = [[k]]
keys nodo@(Node k _ l m r) = keys_aux nodo []

--e
class Dic k v d | d -> k v where
    vacio :: d
    insertar :: Ord k => k -> v -> d -> d
    buscar :: Ord k => k -> d -> Maybe v
    eliminar :: Ord k => k -> d -> d
    claves :: Ord k => d -> [k]

instance Ord k => Dic [k] v (TTree k v) where
    vacio = E
    insertar = insert
    buscar = search
    eliminar = delete
    claves = keys