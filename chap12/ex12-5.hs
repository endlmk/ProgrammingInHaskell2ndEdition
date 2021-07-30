-- Applicative laws

-- 1
-- id :: a -> a
-- pure id :: f (a -> a)
-- x :: f a
-- pure id <*> x = f a
pure id <*> x = x

-- 2
-- g :: a -> b
-- x :: a
-- pure (g x) :: f b
-- pure g :: f (a -> b)
-- pure x :: f a
-- pure g <*> pure x :: f b
pure (g x) = pure g <*> pure x

-- 3
-- x :: f (a -> b)
-- y :: a
-- pure y :: f a
-- x <*> pure y :: f b
-- \g -> g y ::  (a -> b) -> b 
-- pure (\g -> g y) :: f ((a -> b) -> b)
-- pure (\g -> g y) <*> x :: f b
x <*> pure y = pure (\g -> g y) <*> x

-- 4
-- x :: f (a -> b)
-- y :: f (c -> a)
-- z :: f c
-- (y <*> z) :: f a
-- x <*> (y <*> z) :: f b
-- (.) :: (a -> b) -> (c -> a) -> c -> b
-- pure (.) :: f (a -> b) -> (c -> a) -> c -> b
-- pure (.) <*> x :: f ((c -> a) -> c -> b)
-- pure (.) <*> x <*> y :: f (c -> b)
-- (pure (.) <*> x <*> y) <*> z :: f b
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
