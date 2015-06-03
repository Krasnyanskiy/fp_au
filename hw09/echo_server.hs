import Network
import System.IO
import Control.Concurrent(forkIO)
import Control.Monad
import System.Environment

{-
Реализуйте простой эхо-сервер.
Это программа, которая слушает определенный порт, принимает соединения, и всё, что присылает клиент, отправляет ему обратно.
(2 балла)
-}

send :: Handle -> IO ()  
send h = do 
            forever $ do
                line <- hGetLine h 
                hPutStr h line     
                hFlush h            


main :: IO ()
main = do
           (port:_) <- getArgs
           putStrLn $ "Port " ++ show port ++ " will be used!"
           socket <- listenOn $ PortNumber $ fromIntegral (read port)
           forever $ do 
               handle <- liftM (\(h, _, _) -> h) $ accept socket
               send handle 

	       putStrLn "OK!"

