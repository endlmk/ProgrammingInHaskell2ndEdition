length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- length' [1, 2, 3]
-- 1 + length' [2, 3]
-- 1 + (1 + length' [3])
-- 1 + (1 + (1 + length' []))
-- 1 + (1 + (1 + 0))
-- 3

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n - 1) xs

-- drop' 3 [1, 2, 3, 4, 5]
-- drop' 2 [2, 3, 4, 5]
-- drop' 1 [3, 4, 5]
-- drop' 0 [4, 5]
-- [4, 5] 

init' :: [a] -> [a]
init' [] = []
init' [_] = []
init' (x:xs) = x : init' xs

-- init' [1, 2, 3]
-- 1:init' [2, 3]
-- 1:(2:init' [3])
-- 1:(2:[])
-- [1, 2]
