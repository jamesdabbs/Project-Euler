-- Finds the product of "magic" fractions - fractions of the form ab/bc = a/c
-- for some digits a, b and c
-- solution = (8, 800) (-> 1/100)
import Data.Char

concatDigits :: [Int] -> Int
concatDigits x = (read $ map intToDigit x)

-- We want to check that ab / bc = a / c
-- Rather than mucking around with fractions, we can check that ab*c = a*bc
magicFractions = [(a,c) | a<-r, b<-r, c<-r, a /= c,
    (concatDigits [a,b]) * c == a * (concatDigits [b,c])]
    where 
        r = [1..9]
        
solution = foldl (\(a,b) (c,d) -> (a*c, b*d)) (1,1) magicFractions
