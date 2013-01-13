-- Finds the number of paths from the top left to bottom right corner
-- of an n-by-n square
-- solution = 137846528820
import Euler (choose)

solution n = sum [(n `choose` k)^2 | k <- [0..n]] 
