data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show 

halve :: [a] -> ([a], [a])
halve x = splitAt (length x `div` 2) x

balance :: [a] -> Tree a
balance [x] = Leaf x
balance x = Node (balance l) (balance r)
                where (l, r) = halve x