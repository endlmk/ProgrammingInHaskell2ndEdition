safetail xs = if null xs then [] else tail xs
safetail' xs | null xs = []
             | otherwise = tail xs
safetail'' [] = []
safetail'' xs = tail xs