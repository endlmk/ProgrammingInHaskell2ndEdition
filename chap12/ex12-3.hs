instance Applicative ((->) a) where 
    --pure :: b -> (a -> b)
    pure = const
    -- (<*>) :: (c -> (a -> b)) -> (c -> a) -> (c -> b)
    f <*> g = \x -> f x (g x)

