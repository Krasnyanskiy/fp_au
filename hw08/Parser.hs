-- Список экспорта менять нельзя!
module Parser
    ( Parser
    , pure, (<$>), (<$), (<*>), (<*), (*>)
    , empty, (<|>)
    , satisfy, eof
    , evalParser
    , parserTestOK
    , parserTestFail
    ) where

import Control.Applicative
import Test.HUnit
-- import Data.Foldable.Compat  --doesn't work without it -- потому что нужна новая версия стандартной библиотеки
-- import Data.Foldable(toList)


type Error = Either String -- Можно заменить на Maybe, если есть желание.
newtype Parser lex a = Parser { runParser :: [lex] -> Error (a, [lex]) }


-- (0.5 балла)
evalParser :: Parser lex a -> [lex] -> Error a
evalParser p l = fmap fst $ runParser p l

-- (0.5 балла)
satisfy :: (lex -> Bool) -> Parser lex lex
satisfy p = Parser (\l -> case l of 
                            [] -> Left "Nothing to parse"
                            (x:xs) -> case p x of
                                        True -> Right (x, xs) 
                                        _    -> Left "FALSE"   
                   )         


-- (0.5 балла)
eof :: Parser lex ()
eof = Parser (\l -> case l of
                        [] -> Right ((), [])
                        _  -> Left "False"
             )            

instance Functor (Parser lex) where
    fmap func parser = Parser (\l -> case runParser parser l of
                                Left a -> Left a
                                Right (b, bs) -> Right (func b, bs) 
                              )       



instance Applicative (Parser lex) where
    -- (0.5 балла)
    pure val = Parser (\lex -> Right (val, lex))   
    -- (1.5 балл)
    pa <*> pb = Parser (\lex -> case runParser pa lex of
                                    Left a -> Left a
                                    Right (b, bs) -> case runParser pb bs of  
                                                        Left a -> Left a
                                                        Right (b', bs') -> Right (b b', bs')
                         ) 

instance Alternative (Parser lex) where
    -- (0.5 балла)
    empty = Parser (\a -> Left "DRAGONS!")
    -- (0.5 балла)
    (<|>) pa pb = Parser (\lex -> case runParser pa lex of 
                                    Left a -> runParser pb lex
                                    Right p -> Right p
                         )

parserTestOK :: (Eq a, Show a, Eq lex, Show lex) => Parser lex a -> [lex] -> (a, [lex]) -> Test
parserTestOK (Parser p) s r = p s ~?= pure r

parserTestFail :: (Eq a, Show a) => Parser lex a -> [lex] -> Test
parserTestFail (Parser p) s = TestCase $ assertBool "Parser should fail" $ null $ either (const []) return (p s)
