-- Finds the last x digits of the sum 1^1 + 2^2 + ... + 1000^1000
-- f 10 = 9110846700
pMod b e m 
    | e == 0 = 1
    | otherwise = (b * pMod b (e-1) m) `mod` m
    
f x = sum [pMod i i m | i <- [1..1000]] `mod` m
    where
        m = 10^x
