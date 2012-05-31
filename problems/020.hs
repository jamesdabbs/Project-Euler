-- Finds the sum of the digits of n!
-- sumOfFactorialDigits 100 = 648
import Data.Char (digitToInt)

factorial 0 = 1
factorial n = n * factorial (n-1)

sumOfFactorialDigits n = sum $ map digitToInt (show . factorial $ n)
