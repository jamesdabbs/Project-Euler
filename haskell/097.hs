-- Uses modular exponentiation by squaring to evaluate the last digits of a 
--   large power of 2.
-- solution = 8739992576
import Data.Bits

bMod :: Integer -> Integer -> Integer -> Integer
bMod b 0 m = 1
bMod b e m = case testBit e 0 of
    False -> bMod b' e' m
    True  -> (b * bMod b' e' m) `mod` m
    where 
        b' = b^2
        e' = shiftR e 1

solution :: Integer
solution = ((28433 * bMod 2 7830457 m) + 1) `mod` m
    where m = 10^10
