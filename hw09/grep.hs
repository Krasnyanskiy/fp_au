import Control.Exception(catch)
import Control.Monad
import System.Directory
import System.Environment 
import System.IO  
import qualified Data.List as L

{-
grep принимает строку и от 0 и больше имен файлов, выводит строки, в которых встречается как подстрока переданная первым параметром строчка.
Если один из файлов не существовует, нужно вывести сообщение об ошибке и продолжить работу.
(1.5 балла)
-}

main :: IO ()
main = do 
        (pattern:files) <- getArgs
        r <- forM files (\f -> do
                                fe <- doesFileExist f
                                case fe of 
                                    True -> do 
                                              fContent <- readFile f
                                              forM (lines fContent) (\s -> case (L.isInfixOf pattern s) of 
                                                                            True -> putStrLn $ f ++ ": " ++ s
                                                                            False -> putStr ""
                                                                    )
                                              putStr ""
                                    False -> putStrLn $ "File " ++ f ++ " does not exist!"
                        )   
        putStr ""