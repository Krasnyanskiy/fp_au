module Complex where

-- (2 балл)

data Complex = Complex { real :: Double, im :: Double } deriving Eq

instance Num Complex where
    (+) a b = Complex (real a + real b) (im a + im b)    
    (*) a b = Complex (real a * real b - im a * im b) (real a * im b + im a * real b) 
    fromInteger i = Complex (fromIntegral i :: Double) (0.0 :: Double)
    negate a = Complex (-(real a)) (-(im a))
    abs a = Complex (sqrt $ (real a * real a + im a * im a)) 0  
    
    signum _ = error "Complex: signum isn't defined"

instance Fractional Complex where
    (/) (Complex a b) (Complex c d) = Complex ((a*c + b*d)/(c*c+d*d)) ((b*c-a*d)/(c*c+d*d))     
    fromRational a = Complex (fromRational a) 0

-- show и read должны работать как описано в тестах в Main.hs

instance Show Complex where
    show (Complex 0 0) = "0"
    show (Complex a 0) = show a
    show (Complex 0 (-1)) = "-i"
    show (Complex 0 1) = "i"
    show (Complex 0 b) = show b ++ " * i"
    show (Complex a (-1)) = show a ++ " - i"
    show (Complex a 1) = show a ++ " + i"             
    show (Complex a b) | b > 0 = show a ++ " + " ++ show b ++ " * i"
                       | b < 0 = show a ++ " - " ++ show (negate $ Complex 0 b)

read' :: [String] -> [(Complex, String)]
read' [rp] = [(Complex (read rp :: Double) 0, "")]
read' [im, "*", "i"] = [(Complex 0 (read im :: Double), "")]
read' [rp, "+", "i"] = [(Complex (read rp :: Double) 1, "")]
read' [rp, "-", "i"] = [(Complex (read rp :: Double) (-1), "")]
read' [rp,"+",im,r1,r2] = [(Complex (read rp :: Double) (read im :: Double), "")]
read' [rp,"-",im,r1,r2] = [(Complex (read rp :: Double) (-(read im :: Double)), "")]

instance Read Complex where
    readsPrec _ s = read' $ words s


i :: Complex
i = Complex 0 1 
