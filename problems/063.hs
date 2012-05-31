-- Finds n-th powers with n digits
--
digits :: Integer -> Int
digits = length . show

solutions n = dropWhile ((<n) . digits) . takeWhile ((<=n) . digits) . 
    map (^n) $ [1..]

-- The bound of 21 digits was found experimentally. No solutions exist for 
-- 21 < n < 100.
solution = sum [length . solutions $ n | n <- [1..21]]
