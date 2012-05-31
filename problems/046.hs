-- Finds numbers below n that cannot be expressed as p + 2k^2 (p prime)
-- counterexamplesBelow 10000 = [5777, 5993]
import Data.List
import Euler (isqrt, primes)

gb p k = p + (2*k^2)

examplesBelow n = nub . sort $ [gb p k | p <- takeWhile (<=n) primes, 
    k<-[1..(isqrt n) `div` 2], (gb p k) <= n, odd (gb p k)]
    
counterexamplesBelow n = ([3, 5 .. n] \\ (examplesBelow n)) \\ 
    (takeWhile (<=n) primes)
