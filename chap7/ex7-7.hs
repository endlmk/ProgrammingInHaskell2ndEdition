import Data.Char

type Bit = Int

bit2int :: [Bit] -> Int
bit2int bits = sum [w * b | (w, b) <- zip weights bits]
                where weights = iterate (*2) 1

bit2int' :: [Bit] -> Int
bit2int' = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

addParity :: [Bit] -> [Bit]
addParity bits = (if odd (sum bits) then 1 else 0) : bits

encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

checkParity :: [Bit] -> [Bit]
checkParity (p:d) | p == 1 && odd (sum d) = d
                  | p == 0 && even (sum d) = d
                  | otherwise = error "corrupted."

decode :: [Bit] -> String
decode = map ((chr . bit2int) . checkParity) . chop9

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode