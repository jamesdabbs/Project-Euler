-- Find the sum of all primes that are both left- and right-truncatable
-- solution = 748317
import Data.List
import Euler (isPrime, primes)

-- We'll test each substring (cutting from either side) for primality
subs n = sortBy compareLength $ filter (/="") 
    ([n] ++ (tail . tails $ n) ++ (init . inits $ n))

-- Minor optimization: smaller numbers are easier to test for primality, so 
--   well put those at the front of the list 
compareLength s t = (length s) `compare` (length t)

isTruncatable = all (isPrime . read) . subs 
solution = sum . take 11 $ filter (isTruncatable . show) (drop 4 primes)
