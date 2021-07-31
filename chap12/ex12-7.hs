data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
    -- fmap :: (a -> b) -> Expr a -> Expr b
    fmap f (Var x) = Var (f x)
    fmap _ (Val n) = Val n
    fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
    -- pure :: a -> Expr a
    pure x = Var x
    -- <*> :: Expr (a -> b) -> Expr a -> Expr b
    (Val x) <*> _ = Val x
    (Var f) <*> e = fmap f e
    (Add lf rf) <*> e = Add (lf <*> e) (rf <*> e)  

instance Monad Expr where
    return = pure
    -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
    (Val x) >>= _ = Val x
    (Var x) >>= f = f x
    (Add l r) >>= f = Add (l >>= f) (r >>= f)

