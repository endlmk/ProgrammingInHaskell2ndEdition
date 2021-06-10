altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x:xs) = f x : altMap g f xs

luhn :: [Int] -> Bool
luhn xs = (sum (altMap id (\x -> if (x * 2) > 9 then x * 2 - 9 else x * 2) (reverse xs))) `mod` 10 == 0