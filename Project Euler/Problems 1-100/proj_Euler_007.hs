-- Project Euler
-- https://projecteuler.net/

-- 10001st prime

-- Problem 7
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

-- What is the 10 001st prime number?


-- computes the ceiling of the square root 
-- isqrt 17 = 5
isqrt :: (Integral b, Integral a) => a -> b
isqrt x = ceiling (sqrt(fromIntegral x))

-- Tests if a number is prime by division up to isqrt k
-- Special case included for k = 2
isPrime :: Integral a => a -> Bool
isPrime 2 = True
isPrime k = if k > 2 then null [ x | x <- [2..isqrt k], k `mod` x == 0] else False

-- offset 10001 to 10000
ans :: Integer
ans = [n|n<-[1..200000],  isPrime n]!!10000

-- ans
-- 104743
-- (2.33 secs, 954,934,880 bytes)

-- Congratulations, the answer you gave to problem 7 is correct.

-- You are the 416363rd person to have solved this problem.


