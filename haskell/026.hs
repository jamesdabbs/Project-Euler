-- Finds the value for d < 1000 such that 1/d has the longest
-- repeating decimal cycle
-- longestCycle = (982, 983) (so 1/983 yields a length 982 cycle)
import Data.List (sort)
import Euler (bMod)

-- The length of a decimal cycle in base 10 is the multiplicative order
--   of 10 mod d, that is, the smallest k such that 10^k = 1 (mod d)
cycleLength :: Integer -> Integer
cycleLength d = if null l 
  then 0
  else head l
  where 
    l = filter (\k -> (bMod 10 k d) == 1) $ [1..d]

longestCycle = last . sort $ [(cycleLength d, d) | d<-[2..1000]]



