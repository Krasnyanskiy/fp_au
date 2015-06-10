-- (2 балла)
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import Test.HUnit

import Expr
import Eval

getInt :: Eval Value -> Eval Integer
getInt m = do
              val <- m
              case val of 
                  I i -> return i
                  otherwise -> mzero    


getBool :: Eval Value -> Eval Bool
getBool m = do
	            val <- m
	            case val of 
	              B b -> return b 
	              otherwise -> mzero


if' :: Eval Value -> Eval () -> Maybe (Eval ()) -> Eval ()
if' c t e = do
                condRes <- getBool c
                if condRes then 
                    t
                else 
                    fromMaybe mzero e 

--toB :: Integer -> Bool
--toB num | num == 0 = True
--        | otherwise = False                            


evalExpr :: Expr -> Eval Value
evalExpr (Const c) = return c
evalExpr (Var s) = getVar s
evalExpr (BinOp op lhs rhs) = case op of 
                                    And -> do 
                                           	 lb <- getBool $ evalExpr lhs 
                                           	 rb <- getBool $ evalExpr rhs 
                                           	 return $ B $ lb && rb 
                                    Or -> do 
                                            lb <- getBool $ evalExpr lhs 
                                            rb <- getBool $ evalExpr rhs 
                                            return $ B $ lb || rb
                                    otherwise -> do 
                                    	            l <- getInt $ evalExpr lhs
                                                    r <- getInt $ evalExpr rhs
                                                    case op of        	      
					                                    Plus -> return $ I $ l + r  
					                                    Minus -> return $ I $ l - r     
					                                    Mul -> return $ I $ l * r
					                                    Less -> return $ B $ l < r
					                                    Greater -> return $ B $ l > r
					                                    Equals -> return $ B $ l == r
evalExpr (UnOp op nest) =  case op of 
                                Neg -> do  
                                   n <- getInt $ evalExpr nest
                                   return $ I $ negate n
                                Not -> do
                                   n' <- getBool $evalExpr nest 
                                   return $ B $ not $ n'   


evalStatement :: Statement -> Eval ()
evalStatement (Compound body) = mapM_ evalStatement body
evalStatement this@(While cond body) = do 
	                                     val <- getBool $ evalExpr cond
	                                     if val then 
	                                     	evalStatement body >> evalStatement this
	                                     else 
	                                     	return ()
evalStatement (Assign varName expr) = evalExpr expr >>= update varName
evalStatement (If cond thenBody elseBody) = do
	                                            val <- getBool $ evalExpr cond
	                                            if val then 
	                                            	evalStatement thenBody
	                                           	else 
	                                           		fromMaybe mzero (fmap evalStatement elseBody)

------------------------------------------------------------------------------------------------
-- tests
------------------------------------------------------------------------------------------------

test1 = not_ (Var "x") .| Var "y" .< Const (I 3) .& Var "z" .= Var "y" .&
    Const (I 5) .< Var "y" .+ Const (I 7) .* Var "z" .+ Var "y" .* Const (I 3)

test2 = neg (Const $ I 5) .+ neg (Const $ I 3) .* Const (I 2) .- Const (I 7)

test3 = Compound
    [ "r" $= Const (I 1)
    , While (Var "n" .> Const (I 0)) $ Compound
        [ "r" $= Var "r" .* Var "n"
        , "n" $= Var "n" .- Const (I 1)
        ]
    ]

main = fmap (\_ -> ()) $ runTestTT $ test
    [ TestCase $ assertBool "Expected an error" $ errorsCount (runEval (evalExpr test1) $ M.fromList [("x",I 3),("y",I 5),("f",I 5)]) > 0
    , let m = M.fromList [("x",B True),("y",I 5),("z",I 5)] in runEval (evalExpr test1) m ~?= (Just (B False), [], m)
    , let m = M.fromList [("x",B True),("y",I 2),("z",I 2)] in runEval (evalExpr test1) m ~?= (Just (B True ), [], m)
    , runEval (evalExpr test2) M.empty ~?= (Just (I (-18)), [], M.empty)
    , runEval (evalStatement test3) (M.fromList [("n",I 5)]) ~?= (Just (), [], M.fromList [("n",I 0),("r",I 120)])
    ]
  where
    errorsCount (_,es,_) = length es
