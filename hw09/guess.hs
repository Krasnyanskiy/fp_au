import System.Random(randomRIO)
import Control.Monad   
import System.Exit

{-
Реализуйте следующую программу.
Программа загадывает число от 1 до 100, пользователь должен отгадать его.
После каждой попытки программа говорит больше ее число или меньше.
Если пользователь не отгадал за 5 попыток, то проигрыш, иначе победа.
(1.5 балла)
-}

main :: IO ()
main = do	    	
	    secret <- randomRIO (1,100) :: IO Int
	    r <- forM [1..5] (\_ -> do
                            guess <- getLine
                            if (read guess) < secret then
                            	putStrLn "Secret number is >"
                            else if (read guess) > secret then
                                putStrLn "Secret number is <"	
                            else do
                            	putStrLn "WIN!!!"	
                                exitSuccess
	    	              ) 
	    putStrLn "You lose!"