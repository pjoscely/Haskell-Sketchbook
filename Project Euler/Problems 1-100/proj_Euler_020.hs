-- Project Euler
-- https://projecteuler.net/


-- Factorial digit sum

-- Problem 20
-- n! means n × (n − 1) × ... × 3 × 2 × 1

-- For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
-- and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

-- Find the sum of the digits in the number 100!

-- Compute factorial 
fact :: (Eq p, Num p) => p -> p
fact 0 = 1
fact 1 = 1
fact n = n*fact (n -1)

-- String version of fact 100
-- "93326215443944152681699238856266700490715968264381621468592
-- 963895217599993229915608941463976156518286253697920827223758
-- 251185210916864000000000000000000000000"
fact_100 :: String
fact_100 = show$fact 100

-- convert_list "3456789" -> [3,4,5,6,7,8,9]
convert_to_list :: String -> [Int]
convert_to_list = map (read . return) . concat . lines


-- convert to Int list
-- [9,3,3,2,6,2,1,5,4,4,3,9,4,4,1,5,2,6,8,1,6,9,9,2,3,8,8,5,6,2,6,
-- 6,7,0,0,4,9,0,7,1,5,9,6,8,2,6,4,3,8,1,6,2,1,4,6,8,5,9,2,9,6,3,8,9,
-- 5,2,1,7,5,9,9,9,9,3,2,2,9,9,1,5,6,0,8,9,4,1,4,6,3,9,7,6,1,5,6,5,1,8,
-- 2,8,6,2,5,3,6,9,7,9,2,0,8,2,7,2,2,3,7,5,8,2,5,1,1,8,5,2,1,0,9,1,6,8,
-- 6,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

fact_100_list :: [Int]
fact_100_list = convert_to_list fact_100

-- compute sum
ans :: Int
ans = sum fact_100_list

-- ans
-- 648
-- (0.00 secs, 787,616 bytes)

-- Congratulations, the answer you gave to problem 20 is correct.

-- You are the 194842nd person to have solved this problem.

