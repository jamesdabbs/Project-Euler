-- Finds the x-th number that is triangular, pentagonal and hexagonal
-- f 2 = 1533776805
f x = (intersection3 triangulars pentagonals hexagonals)!!x

-- Finds the intersection of two ordered lists
intersection (x:xs) (y:ys) = case (compare x y) of 
    LT ->    intersection xs     (y:ys)
    EQ -> x: intersection xs     ys 
    GT ->    intersection (x:xs) ys
intersection _ _ = []

-- Finds the intersection of three ordered lists
intersection3 xs ys zs = intersection xs (intersection ys zs)

-- The triangular, pentagonal and hexagonal numbers, resp.
triangulars = [x*(x+1)   `div` 2 | x <- [1..]]
pentagonals = [x*(3*x-1) `div` 2 | x <- [1..]]
hexagonals  = [x*(2*x-1)         | x <- [1..]] 
