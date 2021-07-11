import Data.Char

adder :: IO ()
adder = do rest <- getDigit "How many numbers? "
           nums <- sequence (replicate rest (getDigit ""))
           putStrLn ("The Total is " ++ show (sum nums))

getDigit :: String -> IO Int 
getDigit prompt = do putStr prompt
                     x <- getChar 
                     newline
                     if isDigit x then
                         return (digitToInt x)
                     else
                         do putStrLn "ERROR: Invalid digit"
                            getDigit prompt

newline :: IO()
newline = putChar '\n'
