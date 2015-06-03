{-
Реализуйте runBehaviour
(1.5 балла)
-}

data Request  = Get           | Put String
data Response = Result String | OK

type Behaviour = [Response] -> [Request]

prog :: Behaviour
prog ~(OK : x : xs) = Put "more? " : Get : case x of
    Result "no" -> []
    Result "yes" -> prog xs
    _ -> Put "yes or no!" : prog (tail xs)

runHelper :: Behaviour -> [Response] -> [Request] -> Int -> IO ()
runHelper _ _ [] _ = return ()
runHelper bvr resp (req_type:_) iter = case req_type of 
	                                    Get -> do
	                                    	      line <- getLine
	                                    	      let nresp = resp ++ [Result line] 
	                                    	      runHelper bvr nresp (drop iter $ bvr nresp) $ iter + 1
	                                    Put s -> do 
	                                              putStrLn s   
	                                              let nresp = resp ++ [OK]     
	                                              runHelper bvr nresp (drop iter $ bvr nresp) $ iter + 1
runBehaviour :: Behaviour -> IO ()
runBehaviour bvr = runHelper bvr [] (bvr []) 1 



	               

main :: IO ()
main = runBehaviour prog
