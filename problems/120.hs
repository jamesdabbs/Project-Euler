-- Finds the sum of maximal remainders
-- solution = 333082500
r a n = ((a-1)^n + (a+1)^n) `mod` a^2
rMax a = maximum [r a n | n<-[1..2*a]]
solution = sum . map rMax $ [3..2000]
