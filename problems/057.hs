-- Finds how many of the first 1000 approximations to sqrt(2) have more digits 
--   in their numerator than in their denominator
-- solution = 153

-- We iterate by adding one, taking the reciprocal and adding one, 
--   ex. 3/2 -> 5/2 -> 2/5 -> 7/5
-- In general, a/b -> (a+b)/b -> b/(a+b) -> (a+2b)/(a+b)
iter (a, b) = (a + 2*b, a + b)

iters = (3,2) : map iter iters

passes (a,b) = (length . show $ a) > (length . show $ b)

solution = length . filter passes . take 1000 $ iters
