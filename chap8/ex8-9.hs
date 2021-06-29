data Expr = Val Int | Add Expr Expr | Mult Expr Expr

type Cont = [Op]
data Op = EVAL_ADD Expr | EVAL_MULT Expr | ADD Int | MULT Int 

eval :: Expr -> Cont -> Int 
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL_ADD y : c)
eval (Mult x y) c = eval x (EVAL_MULT y : c)

exec :: Cont -> Int -> Int 
exec [] n = n
exec (EVAL_ADD y : c) n =  eval y (ADD n : c)
exec (EVAL_MULT y : c) n =  eval y (MULT n : c)
exec (ADD n : c) m  = exec c (n + m)
exec (MULT n : c) m  = exec c (n * m)

value :: Expr -> Int 
value e = eval e []