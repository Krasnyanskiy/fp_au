module Expr where

import Combinators
import Test.HUnit
import Data.Char

-- 5.5 баллов

data Value = I Int | B Bool deriving (Eq, Show)
data BinOp = Plus | Mul | Minus | Less | Greater | Equals deriving (Eq, Show)
data UnOp = Neg | Not deriving (Eq, Show)
data Expr = BinOp BinOp Expr Expr | UnOp UnOp Expr | Const Value | If Expr Expr Expr | Var String deriving (Eq, Show)
data Statement = Assign String Expr | While Expr Statement | Compound [Statement] deriving (Eq, Show)

infixr 0 @=
(@=) = Assign
(.+) = BinOp Plus
(.-) = BinOp Minus
(.*) = BinOp Mul
(.<) = BinOp Less
(.>) = BinOp Greater
int = Const . I
bool = Const . B
neg = UnOp Neg

--------------------------------------------------
-- Лексический анализ

data Lexeme
    = Ident String
    | NumberConst Int | BoolConst Bool
    | BinOpSign BinOp
    | NotKeyword | WhileKeyword | IfKeyword | ThenKeyword | ElseKeyword | AssignmentSign | SemicolonSign
    | LPSign | RPSign | LBSign | RBSign
    | UnknownLexeme String
    deriving (Eq, Show)

lexer :: String -> [Lexeme]
lexer "" = []
lexer (x:xs) | isSpace x = lexer xs
lexer xs@(x:_) | isAlpha x || x == '_' =
    let (ident, rest) = span (\x -> isAlphaNum x || x == '_') xs
    in case ident of
        "true" -> BoolConst True : lexer rest
        "false" -> BoolConst False : lexer rest
        "while" -> WhileKeyword : lexer rest
        "if" -> IfKeyword : lexer rest
        "then" -> ThenKeyword : lexer rest
        "else" -> ElseKeyword : lexer rest
        "not" -> NotKeyword : lexer rest
        _ -> Ident ident : lexer rest
lexer xs@(x:_) | isDigit x =
    let (number, rest) = span isDigit xs
    in NumberConst (read number) : lexer rest
lexer xs@(x:_) | isOpSymbol x =
    let (op, rest) = span isOpSymbol xs
    in case op of
        "+" -> BinOpSign Plus : lexer rest
        "*" -> BinOpSign Mul : lexer rest
        "-" -> BinOpSign Minus : lexer rest
        "<" -> BinOpSign Less : lexer rest
        ">" -> BinOpSign Greater : lexer rest
        "==" -> BinOpSign Equals : lexer rest
        ":=" -> AssignmentSign : lexer rest
        ";" -> SemicolonSign : lexer rest
        _ -> UnknownLexeme op : lexer rest
lexer ('(':xs) = LPSign : lexer xs
lexer (')':xs) = RPSign : lexer xs
lexer ('{':xs) = LBSign : lexer xs
lexer ('}':xs) = RBSign : lexer xs
lexer (x:xs) =
    let (lex, rest) = span (not . isValid) xs
    in UnknownLexeme (x:lex) : lexer rest

getErrors :: [Lexeme] -> ([Lexeme], [String])
getErrors [] = ([], [])
getErrors (UnknownLexeme e : ls) =
    let (rs, es) = getErrors ls
    in (rs, e:es)
getErrors (l:ls) =
    let (rs, es) = getErrors ls
    in (l:rs, es)

isOpSymbol :: Char -> Bool
isOpSymbol c = elem c "~!@#$%^&*-+=<>?/:;"

isValid :: Char -> Bool
isValid c = isOpSymbol c || isAlphaNum c || isSpace c || elem c "(){}"

--------------------------------------------------
-- Синтаксический анализ

pNum :: Parser Lexeme Int
pNum = (\(NumberConst a) -> a) <$> (satisfy numerical)
            where 
            numerical lex = case lex of 
                                 NumberConst _ -> True
                                 otherwise -> False

pI :: Parser Lexeme Value
pI = (I <$> pNum) <|> 
     (I <$> (symbol (BinOpSign Minus) *> (fmap negate pNum)))                                        

pValue :: Parser Lexeme Value
pValue = (symbol (BoolConst True) *> (pure $ B True)) <|> 
         (symbol (BoolConst False) *> (pure $ B False)) <|>
         pI    


pIdent :: Parser Lexeme String
pIdent = (\(Ident a) -> a) <$> (satisfy ident)
            where 
            ident lex = case lex of 
                            Ident _ -> True
                            otherwise -> False

pVar :: Parser Lexeme Expr
pVar = Var <$> pIdent 

pConst :: Parser Lexeme Expr
pConst = Const <$> pValue

pUnOp :: Parser Lexeme Expr
pUnOp = UnOp <$> ((symbol (BinOpSign Minus) *> pure Neg) <|> (symbol NotKeyword *> pure Not)) <*> pExpr  

pPlus :: Parser Lexeme BinOp
pPlus = symbol (BinOpSign Plus) *> pure Plus

pMinus :: Parser Lexeme BinOp
pMinus = symbol (BinOpSign Minus) *> pure Minus

pMul :: Parser Lexeme BinOp
pMul = symbol (BinOpSign Mul) *> pure Mul

pLess :: Parser Lexeme BinOp
pLess = symbol (BinOpSign Less) *> pure Less

pGreater :: Parser Lexeme BinOp
pGreater = symbol (BinOpSign Greater) *> pure Greater

pEquals :: Parser Lexeme BinOp
pEquals = symbol (BinOpSign Equals) *> pure Equals

pIf :: Parser Lexeme Expr
pIf = (\_ condExpr _ thenExpr _ elseExpr -> If condExpr thenExpr elseExpr) 
      <$> 
      symbol IfKeyword <*> pExpr <*> symbol ThenKeyword <*> pExpr <*> symbol ElseKeyword <*> pExpr

pLex :: Parser Lexeme Expr
pLex =  (Const <$> pValue) <|> pVar <|> pUnOp <|> pIf <|> between (symbol LPSign) (symbol RPSign) pExpr

pFold :: Parser Lexeme Expr
pFold = foldl1P (\lhs binOp rhs -> BinOp binOp lhs rhs) pLex pMul

pSum :: Parser Lexeme Expr
pSum = foldl1P (\lhs binOp rhs -> BinOp binOp lhs rhs) pFold (pPlus <|> pMinus)

pExpr :: Parser Lexeme Expr
pExpr = foldl1P (\lhs binOp rhs -> BinOp binOp lhs rhs) pSum (pLess <|> pGreater <|> pEquals)


--data Statement = Assign String Expr | While Expr Statement | Compound [Statement] deriving (Eq, Show)

pAssign :: Parser Lexeme Statement
pAssign = undefined

pWhile :: Parser Lexeme Statement
pWhile = undefined

pCompound :: Parser Lexeme Statement
pCompound = undefined


pStatement :: Parser Lexeme Statement
pStatement = pAssign <|> pWhile <|> pCompound 

-- tests

testsValue = [ parserTestOK pValue (lexer "123")   === (I 123, [])
             , parserTestOK pValue (lexer "-123")  === (I (-123), [])
             , parserTestOK pValue (lexer "true")  === (B True, [])
             , parserTestOK pValue (lexer "false") === (B False, [])
             ]

testsExpr = [ parserTestOK pExpr (lexer "123 * x")                 === (int 123 .* Var "x", [])
            , parserTestOK pExpr (lexer "123 + x * 5")             === (int 123 .+ (Var "x" .* int 5), [])
            , parserTestOK pExpr (lexer "if x > 0 then x else -x") === (If (Var "x" .> int 0) (Var "x") (neg (Var "x")), [])
            ]

testsStatement = [ parserTestOK pStatement (lexer "x := 3;")                                                   === ("x" @= int 3, [])
                 , parserTestOK pStatement (lexer "while (x > 0) x := x - 1;")                                 === (While (Var "x" .> int 0) $ "x" @= Var "x" .- int 1, [])
                 , parserTestOK pStatement (lexer "r := 1; i := 0; while (i < n) { i := i + 1; r := r * i; }") === (fac, [])
                 ]
  where
    fac = Compound
        [ "r" @= int 1
        , "i" @= int 0
        , While (Var "i" .< Var "n") $ Compound
            [ "i" @= Var "i" .+ int 1
            , "r" @= Var "r" .* Var "i"
            ]
        ]

(===) = id

main = fmap (const ()) $ runTestTT $ test
    $  label "Value"     testsValue
    ++ label "Expr"      testsExpr
    ++ label "Statement" testsStatement
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
