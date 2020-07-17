{-
-- Project Euler
-- https://projecteuler.net/
'''
Pandigital prime
  
Problem 41
We shall say that an n-digit number is pandigital if it makes 
use of all the digits 1 to n exactly once. 
For example, 2143 is a 4-digit pandigital and is also prime.

What is the largest n-digit pandigital prime that exists?
'''
-- ***************************************************************************************
-- Python solution first

#Generates all permutations 
from itertools import permutations
import math
#Tests if number is prime
def is_prime(n):
    if n < 2:
        return False
    if n % 2 == 0 and n > 2: 
        return False
    for i in range(3, int(math.sqrt(n)) + 1, 2):
        if n % i == 0:
            return False
    return True 

#Generate all permuations of 1..7
perm_digits = [''.join(p) for p in permutations('1234567')] 

#Sort descending
perm_digits.sort(reverse=True)

#Print first prime found in the sorted list
for n in perm_digits:
    if(is_prime(int(n))):
        print(n)
        break
'''
7652413

Congratulations, the answer you gave to problem 41 is correct.

You are the 66223rd person to have solved this problem.
-}

-- ***************************************************************************************
-- Haskell solution

import Data.List

-- computes the ceiling of the square root 
-- isqrt 17 = 5
isqrt :: (Integral b, Integral a) => a -> b
isqrt x = ceiling (sqrt(fromIntegral x))

-- Tests if a number is prime by division up to isqrt k
-- Special case included for k = 2
isPrime :: Integral a => a -> Bool
isPrime 2 = True
isPrime k = if k > 2 then null [ x | x <- [2..isqrt k], k `mod` x == 0] else False

-- Sorted descending list of permutations of "1234567"
--  take 3  perm_7 -> ["7654321","7654312","7654231"]
perm_7 :: [[Char]]
perm_7 = reverse$sort$permutations "1234567"


-- Convert perm_7 to integers 
-- take 3 perm_7_int -> [7654321,7654312,7654231]
perm_7_int :: [Int]
perm_7_int = [read n::Int|n<-perm_7]

-- Compute answer
answer :: Int
answer = head [n|n<-perm_7_int, isPrime n]

-- main -> 7652413
-- (0.02 secs, 7,185,176 bytes)
main :: IO ()
main = do  
    putStrLn$show answer





