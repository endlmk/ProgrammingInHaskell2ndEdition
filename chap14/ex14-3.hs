instance Foldable Maybe where
    -- fold :: Monoid a => Maybe a -> a
    fold Nothing = mempty
    fold Just x = x 
    -- foldMap :: Monoid b => (a -> b) -> Maybe a -> b
    foldMap _ Nothing = mempty 
    foldMap f Just x = f x
    -- foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr _ v Nothing = Nothing 
    foldr f v Just x = f x v
    -- foldl :: (a -> b -> a) -> a -> [b] -> a
    foldr _ v Nothing = Nothing 
    foldr f v Just x = f v x

instance Traversable Maybe where
    -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f Maybe b
    traverse g Nothing = pure Nothing 
    traverse g Just x = pure Just <*> g x
