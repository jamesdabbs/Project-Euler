-- This module contains several utility functions that are used in several 
-- problems - like factorizing integers, finding primes, etc.
module Euler
( isInt
, isqrt
, factorial
, choose
, bMod
, factors
, isPrime
, primes
, ominus
, phi
) where

import Data.Bits
import qualified Data.Map as Map

-- Determines if a number is an integer
isInt :: Float -> Bool
isInt x = floor x == ceiling x

-- Finds the integer part of the square root of a number
isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

-- Factorial
factorial 0 = 1
factorial n = n * (factorial (n-1))

-- n `choose` k
choose n k = product [n-(k-1) .. n] `div` product [1 .. k]

-- Binary Power Mod
bMod :: Integer -> Integer -> Integer -> Integer
bMod b 0 m = 1
bMod b e m = case testBit e 0 of
    False -> bMod b' e' m
    True  -> (b * bMod b' e' m) `mod` m
    where 
        b' = b^2
        e' = shiftR e 1

-- Factorization
leastFactor n = head [m | m <- [2 .. n], n `mod` m == 0]
factors n
    | factor == n = [n]
    | otherwise   = [factor] ++ factors (n `div` factor)
    where
        factor = leastFactor n

-- Naive prime test
isPrime x = (x>1) && (null [i | i <- [2 .. isqrt x], x `mod` i == 0])

-- Builds an infinite list of primes
-- See http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf for a discussion of
--   this sieve and a comparison to the naive sieve used in #7
-- The main idea is to store a table with keys being a location and values 
--   a promise to continue filtering from that location
sieve xs = sieve' xs Map.empty
    where
        sieve' []     table = []
        sieve' (x:xs) table = case Map.lookup x table of 
            -- Include x. Promise to remove all multiples of x starting from x^2
            Nothing -> x : sieve' xs (Map.insert (x*x) [x] table)
            -- Remove x. Update the promised filters
            Just facts -> sieve' xs (foldl reinsert (Map.delete x table) facts)
            where
                reinsert table prime = Map.insertWith (++) (x+prime) [prime] table
primes = sieve [2 ..]

-- Tools for ordered lists
ominus (x:xs) (y:ys) = case x `compare` y of
    LT -> x : xs `ominus` (y:ys)
    EQ -> xs `ominus` ys
    GT -> (x:xs) `ominus` ys
ominus l [] = l
ominus [] _ = []
