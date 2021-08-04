data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show 

instance Foldable Tree where 
    -- fold :: Monoid a => Tree a -> a
    fold Leaf = mempty 
    fold (Node l x r) = fold l `mappend` x `mappend` fold r
    -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
    foldMap Leaf = mempty 
    foldMap g (Node l x r) = foldMap g l `mappend` g x `mappend` foldMap g r
    -- foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr _ v Leaf = v
    foldr g v (Node l x r) = foldr g (foldr g (g x v) r) l
    -- foldl :: (a -> b -> a) -> a -> Tree b -> a
    foldl _ v Leaf = v
    foldl g v (Node l x r) = foldl g (foldl g (g v x) l) r

instance Traversable Tree where 
    -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse _ Leaf = pure Leaf
    traverse g (Node l x r) = pure Node <*> traverse g l <*> g x <*> traverse g r