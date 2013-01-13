-- Tools for finding quadratics of the form n^2 + an + b which output primes 
-- for a n = 0, 1 ...
-- bestRun 1000 = (71, (-61,971)), corresponding to the 71-prime run of 
-- n^2 - 61n + 971. The product of the coefficients is -59231.

import Data.List
import Euler (primes)

isPrime a = a `elem` (takeWhile (<=a) primes)

-- (a,b) represents the quadratic n^2 + a*n + b
run (a,b) = takeWhile (isPrime . snd) [(i, ev i) | i <- [0..]]
    where 
        ev i = i*i + a*i + b
runLength = length . run

-- Produces quadratics with coefficients less than x (in abs)
-- - Optimization: since 0 + a*0 + b must be prime, we may assume b is prime
quadratics x = [(a,b) | a <- [-(x-1) .. (x-1)], b <- takeWhile (<x) primes]

bestRun x = maximumBy (\(r1, _) (r2, _) -> r1 `compare` r2)
    (zip (map (runLength) (quadratics x)) (quadratics x)) 

