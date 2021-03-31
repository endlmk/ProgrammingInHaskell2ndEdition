and' :: [Bool] -> Bool 
and' [] = True 
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n e = e : replicate' (n - 1) e

(!!^) :: [a] -> Int -> a
(!!^) (x:xs) 0 = x
(!!^) (x:xs) n = (!!^) xs (n - 1)

elem' :: Eq a => a  -> [a] -> Bool 
elem' e [] = False 
elem' e (x:xs) | e == x = True 
               | otherwise = elem' e xs
