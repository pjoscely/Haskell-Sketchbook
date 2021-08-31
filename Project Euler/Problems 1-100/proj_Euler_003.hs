-- Project Euler
-- https://projecteuler.net/

-- Largest prime factor
-- Problem 3
 
-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?

-- computes the ceiling of the square root function
-- example: isqrt 17 = 5	whereas isqrt 16 = 4	
isqrt :: (Integral b, Integral a) => a -> b
isqrt x = ceiling (sqrt(fromIntegral x))

-- Tests if a number is prime by division up to isqrt k
isPrime :: Integral a => a -> Bool
isPrime k = if k > 1 then null [ x | x <- [2..isqrt k], k `mod` x == 0] else False

-- Generate divisor list
list_divs :: Integral a => a -> [a]
list_divs m = [ n| n <- [2..isqrt m],m `mod` n == 0]

divs = [71,839,1471,6857,59569,104441,486847]

-- List of prime divisors
prime_facs = filter isPrime [71,839,1471,6857,59569,104441,486847]

-- take the maximum of the prime divisors

ans = maximum [71,839,1471,6857]
-- ans = 6857

