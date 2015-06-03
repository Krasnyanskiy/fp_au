import Network
import System.IO
import Control.Concurrent(forkIO)
import Control.Monad
import System.Environment

{-
Реализуйте простой телнет-клиент.
Это программа, которая коннектится по указанному порту и отправляет весь ввод с stdin, а ответ выводит в stdout.
(2 балла)
-}

sendThere :: Handle -> IO ()
sendThere h = do
                forever $ do
	                line <- getLine
	                hPutStr h line 


listenHere :: Handle -> IO ()
listenHere h = do
	             forever $ do
	                 line <- hGetLine h
	                 putStrLn line

main :: IO ()
main = do
	       handle <- connectTo "127.0.0.1" $ PortNumber 12345
	       forkIO $ sendThere handle
	       listenHere handle