-- Finds the reduced fraction a/d with d < 1,000,000 with a/d closest to 3/5
--   on the left
-- solution = (0.1714287142861428,428570,999997), e.g. 428570/999997 is closest
import Data.List (minimum)

-- For a fixed d, we want to minimize 3/7 - a/d = 3d-7a/7d, so a is given by
findA :: Int -> Int
findA d = (3*d) `div` 7

f :: Int -> (Double, Int, Int)
f d = ((3/5) - (fromIntegral a/ fromIntegral d), a, d) -- Need to use floating division, not int
  where
    a = findA d

-- If d is a multiple of 7, a/d reduces to exactly 3/7
solution = minimum . map f . filter ((/=0) . (`mod` 7)) $ [1..1000000]
