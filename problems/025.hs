-- Finds the first Fibonacci number above x
-- firstTermWithNDigits 1000 = 4782
fibs = 1 : 1 : zipWith (+) fibs (tail fibs) 
ifibs = zip [1..] fibs
firstTermWithNDigits n = fst . head . filter ((>=10^(n-1)) . snd) $ ifibs
