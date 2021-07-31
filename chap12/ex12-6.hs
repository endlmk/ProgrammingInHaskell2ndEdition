instance Monad ((->) a) where
    return = pure
    -- (>>=) :: (a -> b) -> (b -> (a -> c)) -> (a -> c)
    f >>= g = \a -> g (f a) a