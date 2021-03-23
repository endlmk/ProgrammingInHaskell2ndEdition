scalarProduct :: [Int] -> [Int] -> Int
scalarProduct x y = sum [e1 * e2 | (e1, e2) <- zip x y ]