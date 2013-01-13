-- Finds the amicable numbers less than n
-- sum . amicablesUntil $ 10000 = 31626
d n = sum [i | i <- [1 .. (n-1)], n `mod` i == 0]
amicablesUntil n = [i | i <- [1..n], d i /= i, d (d i) == i]
