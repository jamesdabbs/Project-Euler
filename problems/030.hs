-- Finds the sum of the fifth powers of the digits of a number 
-- solution = 
import Data.Char

sumOfDigitPowers :: Int -> Int
sumOfDigitPowers = sum . map ((^5) . digitToInt) . show

-- Since 9^5 = 59049, we would expect x >> sumOfDigitPowers x for x > 1000000
-- This bound could likely be sharpened
solution = sum . filter (\x -> x == sumOfDigitPowers x) $ [2 .. 1000000]
