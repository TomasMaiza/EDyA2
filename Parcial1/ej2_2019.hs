data Treap p k = E | N (Treap p k) p k (Treap p k) deriving Show

treap = N (N (N E 2 'a' E) 4 'c' (N E 0 'e' E)) 9 'h' (N E 7 'j' E)

key :: Treap p k -> k
key (N _ _ k _) = k

priority :: Treap p k -> p
priority (N _ p _ _) = p

maximumKey :: (Ord k, Ord p) => Treap p k -> k
maximumKey (N _ _ k E) = k
maximumKey (N _ _ _ r) = maximumKey r

minimumKey :: (Ord k, Ord p) => Treap p k -> k
minimumKey (N E _ k _) = k
minimumKey (N l _ _ _) = minimumKey l

isTreap :: (Ord k, Ord p) => Treap p k -> Bool
isTreap E = True
isTreap (N E p k E) = True
isTreap (N l p k E) = p >= priority l && k >= maximumKey l && isTreap l
isTreap (N E p k r) = p >= priority r && k <= minimumKey r && isTreap r
isTreap (N l p k r) = p >= priority l && p >= priority r && k >= maximumKey l && k <= minimumKey r && isTreap l && isTreap r

rotateL (N l' p' k' (N l p k r)) = N (N l' p' k' l) p k r
rotateR (N (N l p k r) p' k' r') = N l p k (N r p' k' r')