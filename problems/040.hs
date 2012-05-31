-- Finds the product of the 1, 10, 100 ... 1000000th digits of 
--   0.123456789101112131415161718192021...
-- solution = 210
import Data.Char

solution = product [digitToInt $ decimals!!i | i<-map (10^) [0..6]]
    where decimals = foldr (++) "" . map show $ [0..]
