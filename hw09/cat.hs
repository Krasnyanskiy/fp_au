import Control.Exception(catch)
import Control.Monad
import System.Directory
import System.Environment 
import System.IO  

{-
cat принимает имена файлов и выводит их содержимое на экран.
Если в cat не передаются параметры, то она копирует stdin в stdout.
Если один из файлов не существовует, нужно вывести сообщение об ошибке и продолжить работу.
(1.5 балла)
-}

main :: IO ()
main = do 
        files <- getArgs
        case files of
            [] -> forever $ getLine >>= \x -> putStrLn $ show x  
            xs -> forM files (\f -> do
                                fe <- doesFileExist f
                                case fe of 
                                    True -> do 
                                              fContent <- readFile f
                                              mapM putStrLn (lines fContent) 
                                              putStr ""
                                    False -> putStrLn $ "File " ++ f ++ " does not exist!"
                             )   
        putStr ""