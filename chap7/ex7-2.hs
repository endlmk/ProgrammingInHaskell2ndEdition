all' :: (a -> Bool) -> [a] -> Bool 
all' p = foldr (\e b -> p e && b) True 
all'' :: (a -> Bool) -> [a] -> Bool
all'' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool 
any' p  = foldr (\e b -> p e || b) False 
any'' :: (a -> Bool) -> [a] -> Bool
any'' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x:takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x:xs