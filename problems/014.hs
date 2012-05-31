-- Find the number < x that starts the longest Collatz sequence
-- f 1000000 = 837799
import qualified Data.Map as Map
import Data.List 

collatz x
    | odd x     = 3*x + 1
    | otherwise = x `div` 2

collatzLength' x
    | x == 1    = 1
    | otherwise = 1 + (collatzLength' . collatz $ x)

collatzLength value table  = case Map.lookup value table of 
    Nothing     -> collatzLength' value
    Just length -> length
        
collatzTable x = foldl 
    (\table value -> Map.insert value (collatzLength value table) table) 
    Map.empty [1..x]
    
longestSequence x = fst . head . sortBy compareSnd $ 
    (Map.toList . collatzTable $ x)
    where
        compareSnd a b = (snd b) `compare` (snd a)
        
