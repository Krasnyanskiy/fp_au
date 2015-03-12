-- 1. fib n вовзращает n-ое число Фибоначчи.
--    Функция должна работать за линейное вермя и определена для всех целых n.
--    Для отрицательных n значение определяется по формуле fib n = fib (n + 2) - fib (n + 1).
--    (1 балл)
fib :: Integer -> Integer
fib n = if n > 0 
	then fib1 0 1 n
	else minus n * fib1 0 1 (-n)
    where 
    	fib1 :: Integer -> Integer -> Integer -> Integer
        fib1 a b 0 = a
        fib1 a b i = fib1 (a+b) a (i-1)

        minus :: Integer -> Integer 
        minus n = if even n then -1 else 1     

-- 2a. Написать функцию, возвращающую количество цифр числа.
--     Для целочисленного деления можете использовать функции div и mod.
--    (0.5 балла)
numberOfDigits :: Integer -> Integer
numberOfDigits n
            | n < 0 = numberOfDigits (-n)
            | div n 10 > 0 = 1 + numberOfDigits (div n 10) 
	        | otherwise = 1   

-- 2b. Написать функцию, возвращающую сумму цифр числа.
--    (0.5 балла)


sumOfDigits :: Integer -> Integer
sumOfDigits n 
            | n < 0 = sumOfDigits (-n) 
            | div n 10 > 0 = mod n 10 + sumOfDigits (div n 10) 
            | otherwise = n

-- 3. gcd' возвращает НОД.
--    (1 балл)
gcd' :: Integer -> Integer -> Integer
gcd' a b 
    | a < b = gcd b a
    | mod a b /= 0 = gcd (mod a b) b
    | otherwise = b      

-- 4. minp p возвращает минимальное по модулю число x такое, что p x == True. Если такого x не существует, minp не завершается.
--    (1 балл)
minp :: (Integer -> Bool) -> Integer
minp p = minp' p 0  
    where 
        minp' :: (Integer -> Bool) -> Integer -> Integer
    	minp' p x = if p x || p (-x) then x else minp' p (x+1)           

-- 5. integral f a b возвращает значение определенного интеграла функции f на отрезке [a,b].
--    Для реализации можете использовать метод трапеций.
--    (2 балла)
integral :: (Double -> Double) -> Double -> Double -> Double
integral f a b = if a /= b 
	             then ((b-a) / (2.0 * ((b-a) / 0.01))) * (integral' f a b a 0.01)
	             else 0.0
    where 
        integral' :: (Double -> Double) -> Double -> Double -> Double -> Double -> Double
        integral' f a0 b0 a h
            | a == a0 = (f a0) + integral' f a0 b0 (a+h) h
            | a >= b0 = (f b0) 
            | otherwise = (2.0 * (f b0)) + integral' f a0 b0 (a+h) h 

-- !!! не работает этот пример
test :: Double
test = integral (\x -> x * x) 0 1

-- 6. Реализуйте оператор примитивной рекурсии rec, используя функцию (-), укажите тип rec.
--    (1 балл)
rec :: t -> (t -> Integer -> t) -> Integer -> t
rec z s n
       | n == 0 = z 
       | otherwise = s (rec z s (n-1)) n

-- 7. Реализуйте факторил при помощи rec.
--    (1 балл)
facRec :: Integer -> Integer
facRec n = rec 1 (*) n

-- 8. Реализуйте факториал при помощи fix.
--    (1 балл)
facFix :: Integer -> Integer
facFix = fix (\f n -> if n <= 1 then 1 else n * f (n-1))
  where fix f = f (fix f)
