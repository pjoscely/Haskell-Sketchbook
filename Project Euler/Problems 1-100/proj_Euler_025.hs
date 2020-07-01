-- Project Euler
-- https://projecteuler.net/

-- 1000-digit Fibonacci number

-- Problem 25
-- The Fibonacci sequence is defined by the recurrence relation:

-- Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
-- Hence the first 12 terms will be:

-- F1 = 1
-- F2 = 1
-- F3 = 2
-- F4 = 3
-- F5 = 5
-- F6 = 8
-- F7 = 13
-- F8 = 21
-- F9 = 34
-- F10 = 55
-- F11 = 89
-- F12 = 144
-- The 12th term, F12, is the first term to contain three digits.

-- What is the index of the first term in the Fibonacci sequence to contain 1000 digits?

-- Fast fibonacci with momization
-- 0,1,1,2,3,5,........
-- note: this generator has an offset of 2, 144 has index 10 not 12 as above
import Data.List
memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 1
         fib 1 = 2
         fib n = memoized_fib (n-2) + memoized_fib (n-1)

-- Lazy list of digit lengths of the Fibonacci sequence 
all_fib :: [Int]
all_fib = map length$ map show$ map memoized_fib [0..]


-- index -> Just 4780
-- (1.50 secs, 141,800,688 bytes)
index :: Maybe Int
index = elemIndex 1000 all_fib 

-- Also by quick trial and error in Prelude>
-- length$show$ memoized_fib 4780 -> 1000
-- length$show$ memoized_fib 4779 -> 999
-- the term is then 4782 due to the offset of 2

-- Congratulations, the answer you gave to problem 25 is correct.

-- You are the 152454th person to have solved this problem.


