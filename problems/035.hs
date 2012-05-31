-- Finds circular primes (primes such that every circular permutation of 
-- digits is also a prime)
-- numCircularBelow 1000000 = 55

import Data.List
import Euler (isPrime, primes)

-- Gets the (proper) circular permutations of a number by recombining the 
-- heads and tails of the number (as a string)
cPermutations :: Integer -> [Integer]
cPermutations n = filter (/= n) (map (read) 
    (zipWith (++) (tails . show $ n) (inits . show $ n)))

circular = null . filter (not . isPrime) . cPermutations

numCircularBelow x = length [p | p <- takeWhile (<x) primes, circular p]
