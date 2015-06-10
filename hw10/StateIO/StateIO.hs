-- (1.5 балла)
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StateIO
    ( StateIO
    , runStateIO, execStateIO, evalStateIO
    ) where

import Control.Monad.State
import Data.IORef

newtype StateIO s a = StateIO { getStateIO :: IORef s -> IO a }

instance Monad (StateIO s) where
    return arg = StateIO $ \_ -> return arg
    (>>=) (StateIO state) f = StateIO $ \ref -> state ref >>= (\arg -> let (StateIO f') = f arg in f' ref)

instance MonadState s (StateIO s) where
    get = StateIO $ \ref -> readIORef ref
    put arg = StateIO $ \ref -> writeIORef ref arg

runStateIO :: StateIO s a -> s -> IO (a,s)
runStateIO (StateIO f) state = do 
                                 ref <- newIORef state
                                 val <- f ref
                                 state' <- readIORef ref
                                 return (val, state')

execStateIO :: StateIO s a -> s -> IO s
execStateIO state s  = do 
                      (_, s') <- runStateIO state s
                      return s'

evalStateIO :: StateIO s a -> s -> IO a
evalStateIO state s = do 
                        (a, _) <- runStateIO state s
                        return a