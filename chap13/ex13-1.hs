import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char 
item = P (\inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x, xs)])

instance Functor Parser where 
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P (\inp -> case parse p inp of
                                [] -> []
                                [(v, out)] -> [(f v, out)])

instance Applicative Parser where 
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v, inp)])
    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> pv = P (\inp -> case parse pg inp of 
                            [] -> []
                            [(g, out)] -> parse (fmap g pv) out)

instance Monad Parser where
    return = pure 
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pv >>= g = P (\inp -> case parse pv inp of
                            [] -> []
                            [(v, out)] -> parse (g v) out) 

three :: Parser (Char, Char)
three = pure g <*> item <*> item <*> item
        where g x y z = (x, z)

three' :: Parser (Char, Char)
three' = do x <- item
            item
            z <- item
            return (x, z)

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\inp -> [])
    -- (<|>) :: Parser a -> Parser a -> Parser a
    pv <|> pw = P (\inp -> case parse pv inp of
                            [] -> parse pw inp
                            [(v, out)] -> [(v, out)])

sat :: (Char -> Bool) -> Parser Char 
sat p = do x <- item
           if p x then return x else empty 

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower 

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char 
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

comment :: Parser ()
comment = do space
             string "--"
             many (sat (/= '\n'))
             return ()

