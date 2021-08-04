instance Monoid b => Monoid (a -> b) where
    -- mepmpty :: (a -> b)
    mempty = const mempty 
    -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
    f `mappend` g = \x -> f x `mappend` g x