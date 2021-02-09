grid m n = [(x, y) | x <- [0..m], y <- [0..n]]
square n = [t | t <- grid n n, not(fst t == snd t)]