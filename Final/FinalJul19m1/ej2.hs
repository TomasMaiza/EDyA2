
type Name = String
type Path = [String]

data DirTree a = Dir Name [DirTree a] -- nombre de un directorio y su contenido
               | File Name a -- nombre de un archivo y contenido
                deriving (Show, Eq)

dirtree = Dir "/" [Dir "home" [Dir "luis" [File "Carta.txt" "xxx"], Dir "Pedro" [ ]], Dir "mnt" [ ]]

--"/home/pepe" =  ["home", "pepe"]
--"/" = []

--a
--names dirtree = ["home", "mnt"]

names :: [DirTree a] -> [Name]
names [] = []
names (x:xs) = case x of
                Dir name _ -> name : (names xs)
                _ -> names xs

--b
--buscar_arbol :: Name -> [DirTree a] -> DirTree a
--buscar_arbol p [] = File p p
--buscar_arbol p (x:xs) = case x of
--                          Dir name _ -> if name == p then x else buscar_arbol p xs
--                          File _ _ -> buscar_arbol p xs

{-mkdir :: Path -> Name -> DirTree a -> DirTree a
mkdir [] n d = Dir n []
mkdir ["/"] n d = Dir "/" [Dir n []]
mkdir (p:q:ps) n (Dir "/" xs) = if p == "/" then Dir p [mkdir (q:ps) n (buscar_arbol q xs):xs]
                                            else File n n
mkdir (p:ps) n (Dir name xs) = Dir name (mkdir ps n (buscar_arbol p xs):xs) -}

buscar_indice :: Name -> [DirTree a] -> Int
buscar_indice p xs = aux p xs 0
                      where
                        aux p [] _ = -1
                        aux p (x:xs) i = case x of
                                          Dir name ys -> if name == p then i else aux p xs (i + 1)
                                          _ -> aux p xs (i + 1)

mkdir :: Path -> Name -> DirTree a -> DirTree a
mkdir [] n (Dir name xs) = Dir name ((Dir n []):xs)
mkdir ["/"] n (Dir "/" xs) = Dir "/" ((Dir n []):xs)
mkdir ("/":ps) n d = mkdir ps n d
mkdir (p:ps) n (Dir name xs) = let i = buscar_indice p xs
                                   t = xs !! i
                                   l = take i xs
                                   r = drop (i + 1) xs
                                   m = mkdir ps n t
                               in Dir name (l ++ (m : r))

mkDir :: Path -> Name -> DirTree a -> DirTree a
mkDir p n d = mkdir ("/" : p) n d

--c
--ls [ ] dirtree = ["home", "mnt"]
--ls ["home"] dirtree = ["luis", "pedro"]

ls' :: Path -> DirTree a -> [Name]
ls' [] (Dir _ xs) = names xs
ls' ("/":ps) t = ls' ps t
ls' (p:ps) (Dir _ xs) = let i = buscar_indice p xs
                            t = xs !! i
                        in ls' ps t

ls :: Path -> DirTree a -> [Name]
ls p t = ls' ("/" : p) t
