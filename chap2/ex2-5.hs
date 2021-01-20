init' xs = reverse $ tail $ reverse xs

init'' [_] = []    
init'' (x : xs) = x : init'' xs
