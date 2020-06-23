
-- Project Euler
-- https://projecteuler.net/

-- Sum square difference
 
-- Problem 6
-- The sum of the squares of the first ten natural numbers is,

-- 12+22+...+102=385
-- The square of the sum of the first ten natural numbers is,

-- (1+2+...+10)2=552=3025
-- Hence the difference between the sum of the squares of the first ten natural numbers 
-- and the square of the -- sum is 3025−385=2640.

-- Find the difference between the sum of the squares of the first 
-- one hundred natural numbers and the square -- of the sum.


-- Avoiding math formulas
s1 :: Integer
s1 = sum$map (^2) [1..100]

s2 :: Integer
s2 = (sum[1..100])^2

ans :: Integer
ans = abs(s1 - s2)

-- ans
-- 25164150
-- (0.01 secs, 196,296 bytes)

-- Congratulations, the answer you gave to problem 6 is correct.

-- You are the 487476th person to have solved this problem.


