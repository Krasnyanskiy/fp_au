-- (0.5 балла)
import Counter

-- Эти две функции отличаются от обычных тем, что, вызывая tick, они считают сколько шагов заняло их выполнение.
filter' :: (a -> Bool) -> [a] -> Counter [a]
filter' _ [] = return []
filter' p (x:xs) = do 
                    tick
                    calc <- filter' p xs
                    if p x then 
                        return (x:calc)
                    else 
                        return calc     

append :: [a] -> [a] -> Counter [a]
append [] y = return y
append (x:xs) y = do
                    tick
                    appended <- append xs y                      
                    return (x:appended) 

-- Реализуйте qsort через filter' и append
qsort :: Ord a => [a] -> Counter [a]
qsort [] = return []
qsort (x:xs) = do
                lhs <- filter' (< x) xs
                rhs <- filter' (>= x) xs 
                lhs' <- qsort lhs
                rhs' <- qsort rhs
                lhs'' <- append lhs' [x]
                append lhs'' rhs'

-- Первый вызов должен занимать большее количество тиков ~ в 2 раза
main = do
    print $ runCounter $ qsort [1..15]
    print $ runCounter $ qsort [8,4,12,2,6,10,14,1,3,5,7,9,11,13,15]
