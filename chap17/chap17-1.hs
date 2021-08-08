data Expr = Val Int 
          | Add Expr Expr
          | Throw
          | Catch Expr Expr

type Stack = [Int]

eval :: Expr -> Maybe Int 
eval (Val n) = Just n
eval (Add x y) = case eval x of
                    Just n -> case eval y of
                        Just m -> Just (n + m)
                        Nothing -> Nothing 
                    Nothing  -> Nothing
eval Throw = Nothing 
eval (Catch x h) = case eval x of 
                      Just n -> Just n
                      Nothing -> eval h

data Code = HALT | PUSH Int Code | ADD Code | POP Code

comp :: Expr -> Code
comp x = comp' x HALT HALT

comp' :: Expr -> Code -> Code -> Code
comp' (Val n) sc fc = PUSH n sc
comp' (Add x y) sc fc = comp' x (comp' y (ADD sc) (POP fc)) fc
comp' Throw sc fc = fc
comp' (Catch x h) sc fc = comp' x sc (comp' h sc fc)

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (n:s)
exec (ADD c) (m:n:s) = exec c ((n+m) : s)
exec (POP c) (_:s) = exec c s
