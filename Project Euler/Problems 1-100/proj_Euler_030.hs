-- Project Euler
-- https://projecteuler.net/

-- Digit fifth powers
  
-- Problem 30
-- Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

-- 1634 = 1^4 + 6^4 + 3^4 + 4^4
-- 8208 = 8^4 + 2^4 + 0^4 + 8^4
-- 9474 = 9^4 + 4^4 + 7^4 + 4^4
-- As 1 = 1^4 is not a sum it is not included.

-- The sum of these numbers is 1634 + 8208 + 9474 = 19316.

-- Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.


import Data.List  

-- convert_list "3456789" -> [3,4,5,6,7,8,9]
convert_list :: String -> [Int]
convert_list = map (read . return) . concat . lines

-- Helper exponentiation of a list function
-- exp_5 [1,2,3] -> [1,32,243]
exp_5 :: Num a => [a] -> [a]
exp_5 [] = []
exp_5 (x:xs) = (x^5):exp_5 (xs)

-- Convert list of 5-digit numbers to strings 
-- nums_as_strings !! 2345 -> "12345"
-- 6*9^5 = 354294 is upper limit
nums_as_strings :: [String]
nums_as_strings = map show [2..354294]

-- Convert nums_as_strings to list of lists
-- list_of_lists !! 2345 -> [1,2,3,4,5]
list_of_lists :: [[Int]]
list_of_lists = map convert_list nums_as_strings

-- Raise each list element to the 5th power
-- fifth_pow_list !!2345 -> [1,32,243,1024,3125]
fifth_pow_list :: [[Int]]
fifth_pow_list = map exp_5 list_of_lists

-- Create list of sum of powers 
-- list_of_sum_of_powers!!2345 -> 4425
list_of_sum_of_powers :: [Int]
list_of_sum_of_powers = map sum fifth_pow_list

-- this is to avoid potential type conflicts
modify_list_of_sum_of_powers :: [Integer]
modify_list_of_sum_of_powers= map fromIntegral list_of_sum_of_powers

-- create zipped list 
-- zipped!!2345 (2347,18106)
zipped :: [(Integer, Integer)]
zipped = zip [2..354294] modify_list_of_sum_of_powers

-- compare_tuples helper function
compare_tuples :: Eq a => (a, a) -> Bool
compare_tuples (x,y) = x==y

-- build solution list of tuples
-- solns -> [(4150,4150),(4151,4151),(54748,54748),(92727,92727),(93084,93084),(194979,194979)]
solns :: [(Integer, Integer)]
solns = [z|z<-zipped,compare_tuples z == True]

-- produce sum 
sum_of_all :: Integer
sum_of_all = sum$map fst solns 

-- Display the answer
-- 443839
-- (10.44 secs, 10,091,907,960 bytes)
main :: IO ()
main = do  
    putStrLn$show sum_of_all

-- Congratulations, the answer you gave to problem 30 is correct.
-- You are the 107520th person to have solved this problem.

