data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
    fmap g Leaf = Leaf
    fmap g (Node l n r) = Node (fmap g l) (g n) (fmap g r)
    