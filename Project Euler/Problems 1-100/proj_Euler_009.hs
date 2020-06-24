-- Project Euler
-- https://projecteuler.net/

-- Special Pythagorean triplet

-- Problem 9
-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

-- a^2+ b^2 = c^2
-- For example, 32 + 42 = 9 + 16 = 25 = 52.

-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

-- Brute force, one line list comprehension 
ans =  [(a,b,c)|a<-[1..1000],b<-[a+1..1000],let c = 1000 - a - b, a < b + c, b < a + c, c < a + b,  a^2+b^2==c^2]

-- Congratulations, the answer you gave to problem 9 is correct.
-- You are the 353091st person to have solved this problem.

-- ans
-- [(200,375,425)]
-- (0.75 secs, 317,767,720 bytes)
-- 200*375*425 = 31875000
