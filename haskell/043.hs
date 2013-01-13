-- Computes the sum of all 10-digit pandigitals such that each successive 3-digit
--   cluster is divisible by successive primes
-- solution = 16695334890
import Data.Char
import Data.List
import Euler (primes)

subs list = [read . map intToDigit . take 3 . drop i $ list | i <- [1..7]]

solutions = filter passes (permutations [0,1,2,3,4,5,6,7,8,9])
    where
        passes list = all (\(p,n) -> n `mod` p == 0) (zip primes (subs list))
solution = sum . map (read . map intToDigit) $ solutions
