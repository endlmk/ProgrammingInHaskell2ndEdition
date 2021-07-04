isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice (x:xs) y  | z == y = False 
                   | otherwise = isChoice xs z
                    where z = removeElem x y

removeElem :: Eq t => t -> [t] -> [t]
removeElem x [] = []
removeElem x (y:ys) | x == y = ys
                    | otherwise = y: removeElem x ys
