merge :: Ord a => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys) | y < x = y:merge (x:xs) ys
                    | otherwise = x:merge xs (y:ys)

halve :: [a] -> ([a], [a])
halve x = splitAt h x
            where h = (length x) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort x =  merge (msort (fst p)) (msort (snd p)) 
            where p = halve x
