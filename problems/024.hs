import Data.List

lexicographic (a:as) (b:bs) = case a `compare` b of
    GT -> GT
    EQ -> lexicographic as bs
    LT -> LT
    
lPermutations xs = sortBy lexicographic $ permutations xs 

solution = (lPermutations "0123456789")!!999999
