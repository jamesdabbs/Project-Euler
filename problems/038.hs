-- Finds the largest pandigital number formed by concatinating 
-- a*1 ++ a*2 ++ ... ++ a*n for some integers a and n
-- solution = 932718654
import Data.Char
import Data.List 

cProd n a = foldl (++) "" (map (show . (*a)) [1..n])
isPandigital str = (sort str) == "123456789"

-- 9 & (1,2,3,4,5) give us 918273645, so the solution is between this and 
--   987654321 (the largest pandigital number). Thus `a` must begin with a 9
-- Assuming a 2-digit a, a*k has 3 digits for k > 1 and thus we cannont obtain 
--   a 9-digit number by concatinating. By similar reasoning, the solution must
--   have a 4-digit a and n=2.
candidates' = map parse [[9,a,b,c] | a<-d,b<-d,c<-d]
    where
        parse digits = (read . map intToDigit $ digits)::Int
        d = [0..9]
candidates = filter isPandigital $ map (cProd 2) candidates'
solution = maximum candidates
