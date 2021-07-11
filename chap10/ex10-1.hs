putStr :: String -> IO ()
putStr xs = sequence_ [putChar c | c <- xs]