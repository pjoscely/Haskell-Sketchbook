-- Project Euler
-- https://projecteuler.net/

-- Largest prime factor
 
-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?


-- computes the ceiling of the square root 
-- isqrt 17
isqrt :: (Integral b, Integral a) => a -> b
isqrt x = ceiling (sqrt(fromIntegral x))


-- Tests if a number is prime by division up to isqrt k
isPrime :: Integral a => a -> Bool
isPrime k = if k > 1 then null [ x | x <- [2..isqrt k], k `mod` x == 0] else False


-- Generate divisor list up to isqrt
list_divs :: Integral a => a -> [a]
list_divs m = [ n| n <- [2..isqrt m],m `mod` n == 0]

-- possible divisors to try
divs = [71,839,1471,6857,59569,104441,486847]


-- List of prime factors
prime_facs = filter isPrime [71,839,1471,6857,59569,104441,486847]

-- answer appears to be 6857
ans = maximum [71,839,1471,6857]




