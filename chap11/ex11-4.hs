import Data.Char
import Data.List
import System.IO

type Grid = [[Player]]

data Player = O | B | X deriving (Eq, Ord , Show)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Int -> Grid
empty size = replicate size (replicate size B)

full :: Grid -> Bool
full = notElem B . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
         where os = length (filter (== O) ps)
               xs = length (filter (== X) ps)
               ps = concat g


wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
                 line = all (== p)
                 rows = g
                 cols = transpose g
                 dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..length g-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid g =
      putStrLn . unlines . concat . interleave bar . map showRow $ g
      where bar = [replicate (length g * 4 - 1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
                beside = foldr1 (zipWith (++))
                bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < length g^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p =
      [chop (length g) (xs ++ [p] ++ ys) | valid g i]
      where (xs, B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                         return  (read xs)
                   else
                         do putStrLn  "ERROR: Invalid number"
                            getNat prompt

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move:"

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

data Tree a = Node a [Tree a] deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
      | won g = []
      | full g = []
      | otherwise = concat [move g i p | i <- [0..(length g^2-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9


minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
      | wins O g = Node (g, O) []
      | wins X g = Node (g, X) []
      | otherwise = Node (g, B) []
minimax (Node g ts)
      | turn g == O = Node (g, minimum ps) ts'
      | turn g == X = Node (g, maximum ps) ts'
                      where
                            ts' = map minimax ts
                            ps = [p | Node (_, p) _ <- ts']

bestmove :: Grid -> Player -> Tree Grid ->Grid
bestmove g p gt = head [g' | Node (g', p') _ <- ts, p' == best]
               where
                     tree = prune depth 
                     Node (_, best) ts = alphabeta gt

main :: IO ()
main = do hSetBuffering stdout NoBuffering 
          t <- getTurn "Choose your turn(1 or 2)"
          size <- getNat "Enter board size"
          if t == 1 then 
                play (empty size) O (gametree (empty size) O)
          else 
                play (empty size) X (gametree (empty size) X)

play :: Grid -> Player -> Tree Grid -> IO ()
play g p gt = do cls
                 goto (1, 1)
                 putGrid g
                 play' g p gt 

play' :: Grid -> Player -> Tree Grid -> IO ()
play' g p gt
      | wins O g = putStrLn "Player O wins!\n"
      | wins X g = putStrLn "Player X wins!\n"
      | full g = putStrLn "It's a draw!\n"
      | p == O = do i <- getNat (prompt p)
                    case move g i p of
                          [] -> do putStrLn "ERROR: Invalid move"
                                   play' g p gt
                          [g'] -> play g' (next p) gt
      | p == X = do putStr "Player X is thinking..."
                    (play $! (bestmove g p gt)) (next p) gt

-- 11.4 a
getTurn :: String -> IO Int
getTurn prompt = do putStr prompt
                    xs <- getLine 
                    if not (null xs) && (xs == "1" || xs == "2") then
                          return (read xs)
                    else 
                          getTurn prompt

alphabeta :: Tree Grid -> Tree (Grid, Player)
alphabeta (Node g [])
      | wins O g = Node (g, O) []
      | wins X g = Node (g, X) []
      | otherwise = Node (g, B) []
alphabeta (Node g (t:ts))
      | turn' == bp = Node (g, bp) []
      | null ts = alphabeta t
      | otherwise = Node (g, select turn' ps) ts'
      where
            turn' = turn g
            Node (_, bp) _ = alphabeta t
            select p = if p == O then minimum else maximum
            ts' = map alphabeta (t:ts)
            ps = [p | Node (_, p) _ <- ts']
            