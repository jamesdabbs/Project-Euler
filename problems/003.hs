-- Finds the largest prime factor of a number
-- largestFactor 600851475143 = 6857
import Euler (isqrt)

-- Gets the smallest (necessarily prime) factor of x
-- - If we haven't found a factor by isqrt x, then x is prime
smallestFactor x = if null factors 
    then x
    else head factors
    where 
        factors = [y | y <- [2 .. isqrt x], x `mod` y == 0]
           
largestFactor x
    | factor == 1 = x -- x is prime
    | otherwise   = max factor (largestFactor (x `div` factor)) 
    where
        factor = smallestFactor x
