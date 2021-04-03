merge :: Ord a => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys) | y < x = y:merge (x:xs) ys
                    | otherwise = x:merge xs (y:ys)
