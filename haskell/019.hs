-- Finds the number of Sunday the 1sts from 1901-01-01 to 2000-12-31
-- solution = 171
year  = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
year' = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

isLeapYear year = ((year `mod` 4 == 0) && (year `mod` 100 /= 0)) 
    || (year `mod` 400 == 0)
    
expand num = concat . map (\x -> [1..x]) $ list
    where
        list = if isLeapYear num            
            then year'
            else year
            
dates = concat . map expand $ [1901 .. 2000]
-- Jan 1, 1901 was a Tuesday (=2)
days  = drop 1 (cycle [1..7])

matches = length . filter (\(a,b) -> a==7 && b==1) $ zip days dates
