-- Project Euler
-- https://projecteuler.net/

-- Champernowne's constant
  
-- Problem 40
-- An irrational decimal fraction is created by concatenating the positive integers:

-- 0.123456789101112131415161718192021...

-- It can be seen that the 12th digit of the fractional part is 1.

-- If dn represents the nth digit of the fractional part, find the value of the following expression.

-- d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

import Data.List

-- d1 -> 1
d1 = (read . pure :: Char -> Int)$(intercalate ""$ map show [1..])!!0

-- d10 -> 1
d10 = (read . pure :: Char -> Int)$(intercalate ""$ map show [1..])!!9

-- d100 -> 5
d100 = (read . pure :: Char -> Int)$(intercalate ""$ map show [1..])!!99

-- d1000 -> 3
d1000 = (read . pure :: Char -> Int)$(intercalate ""$ map show [1..])!!999

-- d10000 -> 7
d10000 = (read . pure :: Char -> Int)$(intercalate ""$ map show [1..])!!9999

-- d100000 -> 2
d100000 = (read . pure :: Char -> Int)$(intercalate ""$ map show [1..])!!99999

-- d1000000 -> 1
d1000000 = (read . pure :: Char -> Int)$(intercalate ""$ map show [1..])!!999999

-- answer -> 210
answer = d1*d10*d100*d1000*d10000*d100000*d1000000

-- Display the answer
-- main -> 210
main :: IO ()
main = do  
    putStrLn$show answer


-- Congratulations, the answer you gave to problem 40 is correct.

-- You are the 77825th person to have solved this problem.