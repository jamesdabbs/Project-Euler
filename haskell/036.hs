-- Finds numbers which are palindromes in base 2 and base 10
-- sum doublePalindromes = 872187
import Data.Char
import Numeric

rebase i base = showIntAtBase base intToDigit i ""

isPalindrome str = str == reverse str

doublePalindromes = filter doublePalindrome [1 .. 1000000]
    where
        doublePalindrome i = 
            isPalindrome (rebase i 10) &&
            isPalindrome (rebase i 2) 
