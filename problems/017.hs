-- Counts the number of letters in the English spelling of all numbers 1 .. 1000
-- solution = 21124
toEnglish 1    = "one"
toEnglish 2    = "two"
toEnglish 3    = "three"
toEnglish 4    = "four"
toEnglish 5    = "five"
toEnglish 6    = "six"
toEnglish 7    = "seven"
toEnglish 8    = "eight"
toEnglish 9    = "nine"
toEnglish 10   = "ten"
toEnglish 11   = "eleven"
toEnglish 12   = "twelve"
toEnglish 13   = "thirteen"
toEnglish 15   = "fifteen"
toEnglish 18   = "eighteen"
toEnglish 20   = "twenty"
toEnglish 30   = "thirty"
toEnglish 40   = "forty"
toEnglish 50   = "fifty"
toEnglish 60   = "sixty"
toEnglish 70   = "seventy"
toEnglish 80   = "eighty"
toEnglish 90   = "ninety"
toEnglish 1000 = "onethousand"
toEnglish n
    | n < 20           = toEnglish (n-10) ++ "teen"
    | n < 100          = toEnglish (n - (n `mod` 10 )) ++ toEnglish (n `mod` 10)
    | n `mod` 100 == 0 = toEnglish (n `div` 100) ++ "hundred"
    | n < 1000         = toEnglish (n - (n `mod` 100)) ++ "and" ++ 
                            toEnglish(n `mod` 100)
solution = sum . map (length . toEnglish) $ [1..1000]
