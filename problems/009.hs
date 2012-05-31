-- Finds the product of a pythagorean triple (a,b,c) which sums to x (if extant)
-- triple 1000 = 31875000
triple x = head [(x-b-c)*b*c | c <- [1 .. x], b <- [1 .. c], (x-b-c)^2 + b^2 == c^2]
