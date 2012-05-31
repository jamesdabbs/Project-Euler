-- Finds an arithmetic sequence of 4-digit prime numbers that are 
-- digit permuations of each other.
-- solution = 296962999629
import Data.List (sort)
import Euler (primes, isPrime)

-- Tests if n, n+m, n+2m is a sequence where the latter terms are 
-- prime (we'll restrict to n being prime to begin with)
yieldsSeq (n, m) = l!!0 == l!!1 && l!!1 == l!!2
  && isPrime (n+m) && isPrime (n+2*m)
  where 
    l = map (sort . show) $ [n,n+m,n+2*m] 

-- Try out the candidate pairs for (n,m)
(a,b) = head . filter yieldsSeq $ candidates
  where
    ns = takeWhile(<10000) . dropWhile (<1488) $ primes
    candidates = [(n,m) | n<-ns, m<-[100..(10000-n) `div` 2]]

-- And build the 12-digit number you get by concatenating them
solution = concat . map show $ [a, a+b, a+2*b]
