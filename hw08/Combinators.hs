module Combinators
    ( module Parser
    , many, many1
    , symbol, anySymbol, string, oneOf
    , digit, natural, integer
    , spaces
    , try
    , endBy, endBy1
    , sepBy, sepBy1
    , foldr1P, foldl1P
    , between, brackets, parens, braces, angles
    ) where

import Parser
import Data.Char

-- Поведение всех комбинаторов описано в тестах в Main.hs.

-- (0.5 балла)
symbol :: (Eq lex) => lex -> Parser lex ()
symbol l = () <$ satisfy (==l)

-- (0.5 балла)
anySymbol :: Parser lex lex
anySymbol = satisfy (\_ -> True)  

-- (0.5 балла)
digit :: Parser Char Int
digit = pure (digitToInt) <*> satisfy (isDigit)

-- (0.5 балла)
string :: (Eq lex) => [lex] -> Parser lex ()
string s = foldr1 (*>) $ (map symbol s)

-- (0.5 балла)
oneOf :: (Eq lex) => [lex] -> Parser lex lex
oneOf [x]    = satisfy (==x)
oneOf (x:xs) = satisfy (==x) <|> oneOf xs 

-- (0.5 балла)
many :: Parser lex a -> Parser lex [a]
many p = (many1 p) <|> pure []             

-- (0.5 балла)
many1 :: Parser lex a -> Parser lex [a]
many1 p = pure (:) <*> p <*> many p

-- (0.5 балла)
natural :: Parser Char Integer
natural = fmap read $ many1 $ satisfy isDigit

-- (0.5 балла)
integer :: Parser Char Integer
integer = natural <|> (pure (const negate) <*> symbol '-' <*> natural)

-- (0.5 балла)
spaces :: Parser Char ()
spaces = () <$ (many $ symbol ' ')

-- (0.5 балла)
try :: Parser lex a -> Parser lex (Maybe a)
try p = (fmap Just p) <|> pure Nothing 

-- (0.5 балла)
endBy :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy = undefined

-- (0.5 балла)
endBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy1 = undefined

-- (0.5 балла)
sepBy :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy = undefined

-- (0.5 балла)
sepBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy1 = undefined


-- (0.1 балла)
between :: Parser lex a -> Parser lex b -> Parser lex c -> Parser lex c
between pa pb pc = pa *> pc <* pb

-- (0.1 балла)
brackets :: Parser Char a -> Parser Char a 
brackets = between (symbol '[') (symbol ']')  

-- (0.1 балла)
parens :: Parser Char a -> Parser Char a
parens = between (symbol '(') (symbol ')')   

-- (0.1 балла)
braces :: Parser Char a -> Parser Char a
braces = between (symbol '{') (symbol '}')  

-- (0.1 балла)
angles :: Parser Char a -> Parser Char a
angles = between (symbol '<') (symbol '>')  

-- (1 балл)
foldr1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldr1P = undefined

-- (1 балл)
foldl1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldl1P = undefined