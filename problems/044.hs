-- Finds pairs of pentagonal numbers whose sum and difference are also pentagonal
-- solution = 5482660
pentagonals = scanl (+) 1 [4,7..]

-- x is pentagonal iff x = n(3n-1)/2 for some integer n > 0
-- Equivalently, x is pentagonal iff (1 + sqrt(24x+1))/6 is an integer
isPentagonal x = 
    (floor d == ceiling d) &&
    ((1 + (floor d)) `mod` 6 == 0)
    where 
        d = sqrt . fromIntegral $ (24*x + 1)

-- |P_j - P_k| is minimized when |j - k| is minimized, so list the pairs in 
-- increasing order of |j - k|
pairs list  = [(list!!i, list!!j) | i<-[0..], j<-[0..i]]
solutions = filter passes (pairs pentagonals)
    where
        passes (a,b) = isPentagonal (a+b) && isPentagonal (a-b)
solution = diff . head $ solutions
    where
        diff (a,b) = a-b
