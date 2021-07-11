type Board = [Int]

putBoard :: Board -> IO()
putBoard b = sequence_ [putRow n x | (n, x) <- zip [1..] b]

putRow :: Int -> Int -> IO()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))