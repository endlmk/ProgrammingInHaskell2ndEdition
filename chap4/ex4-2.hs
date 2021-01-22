third xs = head $ tail $ tail xs
third' xs = xs !! 2
third''(_:_:a:_) = a