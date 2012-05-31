-- Finds the prime number < 1,000,000 that can be written as a sum of 
-- the largest number of other primes
-- solution = (543, 997651), so 997651 is a sum of 543 consecutive
-- primes
import Data.List (sortBy)
import Euler (primes, isPrime)

-- Finds the sum of m consecutive primes starting from the nth prime
pSum n m = sum . take m . drop n $ primes

-- We only care about runs of length > 21 that add up to less than 
-- 1,000,000 so we can ignore runs that start above 1,000,000 / 21
maxNum = 1000000 `div` 21
maxIndex = length . takeWhile (< maxNum) $ primes

-- The collection of sums, annoted with the length of the run used to
-- reach each one
markedSums = [(m, pSum n m) | n<-[0..maxIndex], 
  m<- takeWhile ((<1000000) . (pSum n)) $ [21..]]

solution = head . filter (\(m,s) -> isPrime s) . sortBy (flip compare) 
  $ markedSums

