-- Finds the sum of the corners in an n-by-n spiral
-- sumOfCorners 1001 = 669171001
diffs = concat . map (take 4 . repeat . (+1)) $ [1, 3 ..]
corners = scanl (+) 1 diffs
sumOfCorners dimensions = sum . takeWhile (<=max) $ corners
    where
        max = dimensions*dimensions
