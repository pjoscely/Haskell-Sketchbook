-- Project Euler
-- https://projecteuler.net/
-- Multiples of 3 and 5
  
-- Problem 1
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
-- The sum of -- these multiples is 23.

-- Find the sum of all the multiples of 3 or 5 below 1000.


-- Tests if an integer is divisible by 3 or 5
div_by_3_or_5 :: Integral a => a -> Bool
div_by_3_or_5 n
    |n `mod` 3  ==  0  ||  n `mod` 5  ==  0  = True
    | otherwise = False

-- Filter list to multiples of 3 or 5
filter_list :: Integral a => [a] -> [a]
filter_list xs = filter div_by_3_or_5 xs

-- sum filtered list
-- total_list :: Integral a => [a] -> a
total_list xs = sum$filter_list xs

-- total_list [3..999] --> 233168
-- (0.01 secs, 931,680 bytes)

-- Congratulations, the answer you gave to problem 1 is correct.

-- You are the 954871st person to have solved this problem.


