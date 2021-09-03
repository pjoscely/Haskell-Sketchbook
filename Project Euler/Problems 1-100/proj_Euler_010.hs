-- Project Euler
-- https://projecteuler.net/

-- Summation of primes
 
-- Problem 10
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

-- Find the sum of all the primes below two million.


-- computes the ceiling of the square root 
-- isqrt 17 = 5
isqrt :: (Integral b, Integral a) => a -> b
isqrt x = ceiling (sqrt(fromIntegral x))

-- Tests if a number is prime by division up to isqrt k
-- Special case included for k = 2
isPrime :: Integral a => a -> Bool
isPrime 2 = True
isPrime k = if k > 2 then null [ x | x <- [2..isqrt k], k `mod` x == 0] else False

ans :: Integer
ans = sum$[n|n<-[2..1999999],  isPrime n]

-- Could be made faster with more faster prime generation algorithm.

-- ans
-- 142913828922
-- (139.06 secs, 51,851,918,520 bytes)

-- Congratulations, the answer you gave to problem 10 is correct.

-- You are the 322962nd person to have solved this problem.

-- You have earned 1 new award:

-- Decathlete: Solve ten consecutive problems
