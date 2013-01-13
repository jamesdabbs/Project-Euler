-- Adds all even terms of the Fibonacci sequence with value < x
-- f 4000000 = 4613732
fibs = 1 : 2 : zipWith (+) fibs (tail fibs) 
f x = sum([y | y <- takeWhile (<x) fibs, y `mod` 2 == 0])
