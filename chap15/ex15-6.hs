
sqroot :: Double -> Double 
sqroot n = head [x | (x, y) <- zip (iterate next 1.0) (tail (iterate next 1.0)), abs (x - y) <= eps]
                where next a = (a + n / a) / 2
                      eps = 0.00001