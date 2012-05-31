-- Finds the least number divisible by all integers up to and including x
-- leastMultiple 20 = 232792560
leastMultiple x = foldl lcm 1 [2 .. x]
