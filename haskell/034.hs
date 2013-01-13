-- Finds the sum of the factorials of the digits of a number 
-- solution = 40730
import Data.Char
import Euler (factorial)

sumOfDigitFactorials :: Int -> Int
sumOfDigitFactorials = sum . map (factorial . digitToInt) . show

-- Since 9! = 362880, we would expect x >> sumOfDigitFactorials x for x > 1000000
-- This bound could likely be sharpened
solution = sum . filter (\x -> x == sumOfDigitFactorials x) $ [10 .. 1000000]
