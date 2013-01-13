-- Finds the difference between the sum of squares and square of sums of the 
-- first x many natural numbers
-- differenceOfSquares 100 = 25164150
differenceOfSquares x = (sum [1..x])^2 - sum (map (^2) [1..x])
