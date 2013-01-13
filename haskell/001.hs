-- Finds sum of all numbers < x which are a multiple of 3 or 5
-- f 1000 = 233168
passes x = x `mod` 3 == 0 || x `mod` 5 == 0
f x = sum $ filter passes [1 .. (x-1)]
