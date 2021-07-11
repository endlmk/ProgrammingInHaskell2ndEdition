type Board = [Int]

putBoard :: Board -> IO()
putBoard b = putBoardHelper b 1

putRow :: Int -> Int -> IO()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoardHelper :: Board -> Int -> IO () 
putBoardHelper [] _ = return ()
putBoardHelper (x:xs) n = do putRow n x 
                             putBoardHelper xs (n + 1) 