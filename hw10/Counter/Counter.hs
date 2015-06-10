-- (0.5 балла)
module Counter
    ( Counter
    , tick
    , runCounter
    ) where

import Control.Applicative
import Control.Monad    	

-- Монада Counter считает количество тиков, т.е. вызовов функции tick
data Counter a = Counter Int a

tick :: Counter ()
tick = Counter 1 () 

-- Возвращает результат вычислений и количество тиков
runCounter :: Counter a -> (a, Int)
runCounter (Counter cnt result) = (result, cnt) 

instance Monad Counter where
    return = Counter 0
    (>>=) counter future = let (work, cnt) = runCounter counter
                               (work_yet, cnt_snd) = runCounter $ future work
                           in Counter (cnt + cnt_snd) work_yet        

