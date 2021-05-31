unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x) 

int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 = unfold null (take 8) (drop 8)

map' f = unfold null (f.head) tail

iterate' f = unfold (\_ -> False) id f
