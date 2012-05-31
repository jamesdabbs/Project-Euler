-- Finds the first n s.t. (p_n + 1)^n + (p_n - 1)^n mod p_n^2 has 10 digits
-- solution = 21035
import Data.List
import Euler (primes)

-- Note that (p_n + 1)^n mod p_n^2 = n p_n + 1.  It follows that
r :: Integer -> Integer
r n
    | odd n     = 2 * n * (genericIndex primes (n-1))
    | otherwise = 2
    
solution :: Integer
solution = head . filter ((>=b) . r) $ [7000 ..]
    where b = 10^10
