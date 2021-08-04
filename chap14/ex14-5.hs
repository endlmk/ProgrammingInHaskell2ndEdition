import Data.Foldable

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p = filter p . toList