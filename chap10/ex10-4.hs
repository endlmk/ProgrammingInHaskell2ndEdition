import Data.Char

adder :: IO ()
adder = do rest <- getDigit "How many numbers? "
           adderHelper 0 rest

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

adderHelper :: Int -> Int -> IO ()
adderHelper sum 0 = putStrLn ("The Total is " ++ show sum)
adderHelper sum rest = do x <- getDigit ""
                          adderHelper (sum + x) (rest - 1)

