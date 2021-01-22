halve xs = (take h xs, drop h xs)
            where h = length xs `div` 2

halve' xs = splitAt (length xs `div` 2) xs