-- Finds perimiters for which there are multiple integral right triangles
-- maxSolutionsUntil 1000 = (840, 8)
import Data.List

trianglesWithPerimeter p = [(p-b-c,b,c) | c<-[2..(p `div` 2)], b<-[1..(c-1)],
    (p-b-c) < b, (p-b-c)^2 + b^2 == c^2]
    
maxSolutionsUntil x = maximumBy compareSecond 
    (zip [12..x] (map (length . trianglesWithPerimeter) [12..x]))
    where compareSecond (_, a) (_, b) = compare a b
