-- Finds all products where each digit 1-9 appears exactly once
-- solution = 45228
import Data.Char
import Data.List
import qualified Data.Set as Set

parse :: [Int] -> Int
parse = read . map intToDigit

-- There are two feasible ways to get a 9-digit pandigital product:
-- ab * cde = fghi -or- a * bcde = fghi
test perm = if a*b == c
    then c
    else if d*e == f
        then f
        else 0
    where
        a = parse (take 2          $ perm)
        b = parse (take 3 . drop 2 $ perm)
        c = parse (take 4 . drop 5 $ perm)
        d = parse (take 1          $ perm)
        e = parse (take 4 . drop 1 $ perm)
        f = parse (take 4 . drop 5 $ perm)

-- This is very brute-force and could likely be optimized, but runs fast enough
-- for this problem
solutions = nub . filter (/= 0) . map test . permutations $ [1,2,3,4,5,6,7,8,9]
solution  = sum solutions
