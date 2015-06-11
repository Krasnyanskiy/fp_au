-- (2 балла)
module Eval
    ( Eval, runEval
    , Error, Store
    , update, getVar
    ) where

import qualified Data.Map as M
import Data.List
import Control.Monad
import Control.Applicative

import Expr

type Error = String
type Store = M.Map String Value

newtype Eval a = Eval { runEval :: Store -> (Maybe a, [Error], Store) }

instance Functor Eval where
    fmap f (Eval m) = Eval $ \store -> let (res, errs, st) = m store
                                       in  (fmap f res, errs, st) 


instance Applicative Eval where
    pure x = Eval $ \store -> (Just x, [], store)
    Eval m <*> Eval k = Eval $ \store -> let (res, errs, st) = m store
                                             (res', errs', st') = k st'
                                         in (res <*> res', errs ++ errs', st')        

instance Monad Eval where
    return x = Eval $ \store -> (Just x, [], store)
    Eval m >>= k = Eval $ \store -> let (res, errs, st) = m store
                                    in 
                                    case res of 
                                      Just x -> let (res', errs', st') = (runEval $ k x) st 
                                                in (res', errs ++ errs', st')
                                      Nothing -> (Nothing, errs, st)


instance Alternative Eval where
    empty = Eval $ \store -> (Nothing, [], store) 
    Eval l <|> Eval r = Eval $ \store -> let (res, errs, st) = l store
                                         in 
                                         case res of 
                                             Just x -> (res, errs, st)
                                             Nothing -> r store  

-- MonadPlus - аналог Alternative для монад
-- mzero - вычисление, которое ничего не делает, сразу завершается неуспехом
-- mplus m1 m2 пытается выполнить m1, если тот завершился неуспехом, выполняет m2
-- Примеры использования этого класса есть в Utils.hs
instance MonadPlus Eval where
    mzero = Eval $ \store -> (Nothing, ["We start and we've failed"], store)
    mplus (Eval l) (Eval r) = Eval $ \store -> let (res, errs, st) = l store
                                               in 
                                               case res of 
                                                   Just x -> (res, errs, st)
                                                   Nothing -> r store 

update :: String -> Value -> Eval ()
update k v = Eval $ \store -> (Just (), [], M.insert k v store)

getVar :: String -> Eval Value
getVar k = Eval $ \store -> case M.lookup k store of 
                                Just x -> (Just x, [], store)
                                Nothing -> (Nothing, ["Nope"], store)   
