import Data.List
import qualified Data.Map as Map

isAbundant n = sum [i | i<-[1..(n-1)], n `mod` i ==0] > n

abundantsBelow n = fst . unzip . Map.toList . populate Map.empty $ [12 .. n]
    where 
        populate table (x:xs) = case Map.lookup x table of
            -- If it's already in the table, we know it's abundant
            Just _  -> populate table xs
            -- Otherwise, check if it's abundant manually
            Nothing -> 
                if isAbundant x
                    -- If so, add it and all multiples (up to n)
                    then populate (foldl (\t v -> Map.insert v 1 t) 
                        table [x, 2*x .. n]) xs
                    else populate table xs
        populate table [] = table
        
sums = nub . sort . filter (<28123) $ [abs!!i + abs!!j | i<-rng, j<-[0..i]]
    where
        abs = abundantsBelow 28123
        rng = [0 .. length abs]
