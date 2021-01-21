second xs = head (tail xs) -- [a] -> a
swap (x, y) = (y, x) -- (a, b) -> (b, a)
pair x y = (x, y) -- a -> b -> (a, b)
double x = x * 2 -- Num a => a -> a
palindrome xs = reverse xs == xs -- Eq a => [a] -> bool
twice f x = f(f x) -- (a -> a) -> a -> a