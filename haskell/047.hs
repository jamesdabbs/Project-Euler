-- Finds the first four consecutive numbers which have four distinct prime 
-- solution = Just(134043, 134046), corresponding to 134043 .. 134046 all having
--    four distinct prime factors
import Data.List
import Euler (factors)

numFactors = length . nub . factors 
fourFactors = filter ((==4) . numFactors) $ [2 ..]
solution = find (\(a,b) -> b==a+3) . zip fourFactors $ drop 3 fourFactors
