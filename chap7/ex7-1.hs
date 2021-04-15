mapFiltered :: (a -> Bool) -> (a -> b) -> [a] -> [b]
mapFiltered p f = map f . filter p 
