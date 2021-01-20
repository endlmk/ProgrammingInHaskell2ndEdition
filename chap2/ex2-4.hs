last' xs = head $ reverse xs
last'' [x] = x
last'' (x : xs) = last'' xs
