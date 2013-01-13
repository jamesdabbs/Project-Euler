-- Finds the 1000000th permuation of "0123456789" (in the lexicographic
-- order)
-- solution = 2783915460
import Data.List

lexicographic (a:as) (b:bs) = case a `compare` b of
    GT -> GT
    EQ -> lexicographic as bs
    LT -> LT
    
lPermutations xs = sortBy lexicographic $ permutations xs 

solution = (lPermutations "0123456789")!!999999
