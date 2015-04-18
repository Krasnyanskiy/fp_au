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
	    forM_ [1..5] $ \_ -> do
                            guess <- getLine
                            case compare secret (read guess) of
                                LT -> putStrLn "Secret number is <"
                                GT -> putStrLn "Secret number is >"	
                            	EQ -> putStrLn "WIN!!!"	>> exitSuccess
	    putStrLn "You lose!"
