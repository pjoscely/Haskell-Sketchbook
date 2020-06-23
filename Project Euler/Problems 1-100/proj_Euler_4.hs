-- Project Euler
-- https://projecteuler.net/


-- Largest palindrome product
    
-- Problem 4
-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit -- numbers is 9009 = 91 × 99.

-- Find the largest palindrome made from the product of two 3-digit numbers.

-- One line list comprehension
ans :: Integer
ans = maximum [ (x*y)|x<- [100..999],y<- [100..999], reverse(show(x*y)) == show (x*y)] 

-- ans
-- 906609
-- (1.18 secs, 809,169,904 bytes)

-- Congratulations, the answer you gave to problem 4 is correct.

-- You are the 480505th person to have solved this problem.