-- infinite recursion
fac :: Int -> Int 
fac n | n < 0 = 0
      | n == 0 = 1
      | otherwise = n * fac (n - 1)
