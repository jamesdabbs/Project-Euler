-- Finds integers such that multiplication by 1..6 just permutes the digits
-- solution = 142857
import Data.Char (digitToInt)
import Data.List (sort)

allEqual list = all (== head list) . tail $ list

digits :: Integer -> [Int]
digits = map digitToInt . show

productDigits x = map (sort .digits . (x*)) $ [1..6]

solution = head . filter (allEqual . productDigits) $ [1..]
