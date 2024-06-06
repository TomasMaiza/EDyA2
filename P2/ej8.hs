{-
f :: c -> d
g :: a -> b -> c

h :: a -> b -> d
h x y = f (g x y)

h = f . g
h x y = (f . g) x y

. :: (a -> b) -> (c -> a) -> b 

}