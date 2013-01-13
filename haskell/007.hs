-- Finds the n-th prime number
-- Note that Euler.hs contains a significantly faster method for finding primes
-- nthPrime 10001 = 104743
primes = 2 : sieve [3, 5 ..]
    where 
        sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
nthPrime n = primes!!(n-1)
