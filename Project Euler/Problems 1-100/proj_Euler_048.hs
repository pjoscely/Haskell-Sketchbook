-- Project Euler
-- https://projecteuler.net/

-- Self powers

-- Problem 48
-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000

-- Compute String version of the 1000 term sum
digits_as_String :: String
digits_as_String = show$sum[p|n<-[1..1000], let p = n^n]

-- length = 3001
-- Compute the string's length
str_length :: Int
str_length = length$digits_as_String 

-- "9110846700"
last_ten_digits :: [Char]
last_ten_digits = drop (str_length - 10) digits_as_String

-- 9110846700
-- Convert to an Int if needed for later calculations
answer_as_Int :: Int
answer_as_Int = read "9110846700" :: Int

-- Display the answer
-- main -> 9110846700
main :: IO ()
main = do  
    putStrLn last_ten_digits

-- Congratulations, the answer you gave to problem 48 is correct.

-- You are the 109773rd person to have solved this problem.

