-- Find the sum of all primes less than x
-- sumOfPrimesBelow 2000000 = 142913828922
import Euler (primes)

sumOfPrimesBelow x = sum $ takeWhile (<x) primes
