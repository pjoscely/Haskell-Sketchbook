{-
-- Project Euler
-- https://projecteuler.net/
Distinct primes factors
 
Problem 47
The first two consecutive numbers to have two distinct prime factors are:

14 = 2 × 7
15 = 3 × 5

The first three consecutive numbers to have three distinct prime factors are:

644 = 2² × 7 × 23
645 = 3 × 5 × 43
646 = 2 × 17 × 19.

Find the first four consecutive integers to have four distinct prime factors each. What is the first of these numbers?
-}

import Data.List
import qualified Data.Set as Set
-- computes the ceiling of the square root 
-- isqrt 17 = 5
isqrt :: (Integral b, Integral a) => a -> b
isqrt x = ceiling (sqrt(fromIntegral x))

-- Tests if a number is prime by division up to isqrt k
-- Special case included for k = 2
isPrime :: Integral a => a -> Bool
isPrime 2 = True
isPrime k = if k > 2 then null [ x | x <- [2..isqrt k], k `mod` x == 0] else False

-- Generate tentative list of primes
primes :: [Integer]
primes = [p| p<-[2..1000], isPrime p]

-- Number of prime factors 
-- num_p_fac 646 -> 3
num_p_fac :: Num a => Integer -> a
num_p_fac n = sum[1| p<-primes, p < n, n`mod`p == 0]

-- Trial error leads to searching in the range 100000...200000
-- answer :: Integer
answer = head [n|n <- [100000..200000], num_p_fac n == 4, num_p_fac (n+1) == 4,num_p_fac (n+2) == 4,num_p_fac (n+3) == 4]

-- main -> 134043
-- (4.40 secs, 1,219,687,992 bytes)
main :: IO ()
main = do  
    (putStrLn.show) answer

-- Congratulations, the answer you gave to problem 47 is correct.

-- You are the 56268th person to have solved this problem.



