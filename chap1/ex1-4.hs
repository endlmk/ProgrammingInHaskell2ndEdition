qsort_dec [] = []
qsort_dec (x:xs) = qsort_dec larger ++ [x] ++ qsort_dec smaller 
                    where 
                        larger = [a | a <- xs, a > x]
                        smaller = [b | b <- xs, b <= x]