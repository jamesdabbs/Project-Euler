-- Finds the number whose square has the form 1_2_3_4_5_6_7_8_9_0
-- solution = 1389019170
import Data.Char (intToDigit)
import Euler (isqrt)

alternates (x:y:zs) = x : alternates zs
alternates (x:_)    = [x]
alternates _        = []

target = (map intToDigit [1..9]) ++ "0"

passes i = (alternates . show $ i^2) == target

-- 1010101010 is isqrt 1020304050607080900
-- Since we know the last digit is a 10, our number has to end with a 0
solution = head . filter passes $ [1010101010, 1010101020 ..]
