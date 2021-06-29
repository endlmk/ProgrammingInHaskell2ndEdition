data Tree a = Leaf a | Node (Tree a) (Tree a)

len :: Tree a -> Int
len (Leaf _) = 1
len (Node l r) = len l + len r

balanced :: Tree a -> Bool 
balanced (Leaf _) = False 
balanced (Node l r) = abs (len l - len r) <= 1 