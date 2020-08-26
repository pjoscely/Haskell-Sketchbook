-- Project Euler
-- https://projecteuler.net/

-- Smallest multiple

-- Problem 5
-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

-- Simply factor in highest prime power
ans = 16*9*5*7*11*13*17*19
-- 232792560
-- (0.01 secs, 121,672 bytes)
-- Congratulations, the answer you gave to problem 5 is correct.

-- You are the 484475th person to have solved this problem.

-- The following is a hack. Should use Haskell arrays not lists for this brute force solution
even_div :: Integral a => a -> Bool
even_div n = (n `mod` 16 == 0) && (n `mod` 9 == 0) && (n `mod` 5 == 0) && (n `mod` 7 == 0) && (n `mod` 11 == 0) 
             && (n `mod` 13 == 0) && (n `mod` 17 == 0)&& (n `mod` 19 == 0)  

num_lst :: Integral a => [a] -> [a]
num_lst xs = filter even_div xs

--minimum$num_lst [232792550..232792570]
-- 232792560
-- (0.01 secs, 136,184 bytes)

-- A fast solution using foldl

-- fast_soln [1..20]
-- 232792560

-- fast_soln [1..200]
-- 337293588832926264639465766794841407432394382785157234228847021917234018060677390066992000
-- (0.01 secs, 325,400 bytes)
fast_soln :: (Foldable t, Integral b) => t b -> b
fast_soln xs = foldl (lcm) 1 xs


