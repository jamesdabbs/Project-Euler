-- Finds the maximal digital sum of a^b for a,b < 100
-- solution = 972
import Data.Char (digitToInt)

digitSum :: Integer -> Int
digitSum = sum . map digitToInt . show

solution = maximum [digitSum (a^b) | a <- [1..99], b <- [1..99]]
