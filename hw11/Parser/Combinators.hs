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

symbol :: (Eq lex) => lex -> Parser lex ()
symbol l = () <$ satisfy (==l)

anySymbol :: Parser lex lex
anySymbol = satisfy (\_ -> True)  

digit :: Parser Char Int
digit = pure (digitToInt) <*> satisfy (isDigit)

string :: (Eq lex) => [lex] -> Parser lex ()
string s = foldr1 (*>) $ (map symbol s)

oneOf :: (Eq lex) => [lex] -> Parser lex lex
oneOf [x]    = satisfy (==x)
oneOf (x:xs) = satisfy (==x) <|> oneOf xs 

many :: Parser lex a -> Parser lex [a]
many p = (many1 p) <|> pure []             

many1 :: Parser lex a -> Parser lex [a]
many1 p = pure (:) <*> p <*> many p

natural :: Parser Char Integer
natural = fmap read $ many1 $ satisfy isDigit

integer :: Parser Char Integer
integer = natural <|> (pure (const negate) <*> symbol '-' <*> natural)

spaces :: Parser Char ()
spaces = () <$ (many $ symbol ' ')

try :: Parser lex a -> Parser lex (Maybe a)
try p = (fmap Just p) <|> pure Nothing 

endBy :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy = undefined

endBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy1 = undefined

sepBy :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy = undefined

sepBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy1 = undefined

between :: Parser lex a -> Parser lex b -> Parser lex c -> Parser lex c
between pa pb pc = pa *> pc <* pb

brackets :: Parser Char a -> Parser Char a 
brackets = between (symbol '[') (symbol ']')  

parens :: Parser Char a -> Parser Char a
parens = between (symbol '(') (symbol ')')   

braces :: Parser Char a -> Parser Char a
braces = between (symbol '{') (symbol '}')  

angles :: Parser Char a -> Parser Char a
angles = between (symbol '<') (symbol '>')  

foldr1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldr1P = undefined

foldl1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldl1P = undefined
