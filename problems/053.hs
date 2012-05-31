-- Counts the number of large binomial coefficients
-- This could certainly be optimized, but still ran wicked fast so no need for 
--   now
-- solution = 4075
choose n k = product [n-(k-1) .. n] `div` product [1 .. k]
solution = length . filter (>1000000) $ [choose n k | n <- [1..100], k <- [1..n]]
