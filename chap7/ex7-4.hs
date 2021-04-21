dec2int :: [Int] -> Int 
dec2int = foldl (\s e -> s * 10 + e) 0