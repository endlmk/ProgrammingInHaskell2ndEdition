map' f  = foldr (\e l -> (f e):l) [] 
filter' p = foldr (\e l -> if p e then e:l else l) []