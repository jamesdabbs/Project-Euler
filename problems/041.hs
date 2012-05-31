-- Generates pandigital numbers and checks if they're prime
-- solution = 7652413
import Data.Char 
import Data.List
import Euler (isPrime)

parse :: [Int] -> Integer
parse = read . map intToDigit

pandigitals' n = map parse . permutations $ [1..n]
    
pandigitals = reverse . sort . foldl (++) [] $ [pandigitals' i | i <- [9,8..2]] 

solution = head . filter isPrime $ pandigitals
