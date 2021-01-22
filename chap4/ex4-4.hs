False || False = False 
_ || _ = True

True || True = True
True || False = True
False || True = True
False || False = False 

False || b = b
True || _  = True

b || c | b == c = b
       | otherwise = True
