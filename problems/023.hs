-- Finds sum of all numbers (<28123) which cannot be written as the 
-- sum of two abundant numbers.
-- solution = 4179871
import Data.Set (Set, fromList, toList, difference, fold, member)
import Data.List (nub)

import Euler(isqrt, isInt)

-- Gets the lower factors of n (up to sqrt(n))
lFactors :: Integer -> [Integer]
lFactors n = filter ((== 0) . (n `mod`)) [1 .. (isqrt n)]

-- Finds the sum of the divisors of n
-- Using only the lower factors is more efficient (O(sqrt n)), but 
--   includes n in the count and double-counts sqrt n if n is square,
--   so we have to compensate for that.
divSum :: Integer -> Integer
divSum n = sum [i + (n `div` i) | i<-lFactors n] - diff
  where
    diff = if isInt . sqrt . fromInteger $ n 
      then n + isqrt n 
      else n

-- Tests if a number is abundant
isAbundant :: Integer -> Bool
isAbundant n = divSum n > n


-- List of abundants in our range
abundants = filter isAbundant [1..28123]
-- List of abundant sums in our range, converted to a set for faster
--   element lookup
abSums = fromList $ [a+b | a<-abundants, 
  b<-takeWhile (<= min a (28123-a)) abundants]
-- The sum of everything in range but not in abSums
solution = sum . filter (not . (`member` abSums)) $ [1..28123]
