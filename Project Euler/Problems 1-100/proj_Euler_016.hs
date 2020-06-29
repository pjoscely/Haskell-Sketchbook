-- Project Euler
-- https://projecteuler.net/

-- Power digit sum
 
-- Problem 16
-- 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

-- What is the sum of the digits of the number 2^1000?

import Data.List  

convert_list :: String -> [Int]
convert_list = map (read . return) . concat . lines

-- sum of list of digits 
total_digits :: Int
total_digits = sum$convert_list$ show (2^1000)

-- Display the answer
-- 1366
-- (0.02 secs, 1,322,432 bytes)
main :: IO ()
main = do  
    putStrLn$show total_digits 

-- Congratulations, the answer you gave to problem 16 is correct.

-- You are the 225461st person to have solved this problem.

