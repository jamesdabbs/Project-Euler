-- Finds the number of ways to combine pence pieces to get 200p
-- length solutions = 73682

-- Pence come in 200, 100, 50, 20, 10, 5, 2 and 1
-- (a1, a2 ..) represents 200*a1 + 100*a2 + ..
ev :: [Integer] -> Integer
ev = sum . zipWith (*) [200, 100, 50, 20, 10, 5, 2, 1]

available :: [Integer] -> Integer -> [Integer]
available list value = [0 .. (200 - (ev list)) `div` value]

candidates = [[a1, a2, a3, a4, a5, a6, a7, a8] |
    a1 <- [0,1],
    a2 <- available [a1]                       100,
    a3 <- available [a1, a2]                    50,
    a4 <- available [a1, a2, a3]                20,
    a5 <- available [a1, a2, a3, a4]            10,
    a6 <- available [a1, a2, a3, a4, a5]         5,
    a7 <- available [a1, a2, a3, a4, a5, a6]     2,
    a8 <- available [a1, a2, a3, a4, a5, a6, a7] 1]
solutions = filter ((==200) . ev) candidates
