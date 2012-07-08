-- Finds the largest palindrome made from the product of 2 x-digit numbers
-- largestPalindrome 3 = 906609
isPalindrome str = str == reverse str

largestPalindrome x = maximum $ filter (isPalindrome . show)
    [a * b | a <- rng, b <- rng]
    where
        rng = [10^(x-1) .. (10^x - 1)]