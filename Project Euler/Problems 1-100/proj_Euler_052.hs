{-

Project Euler
https://projecteuler.net/

Permuted multiples
 
Problem 52
It can be seen that the number, 125874, and its double, 251748, 
contain exactly the same digits, but in a different order.

Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, 
contain the same digits.
-}
import Data.List
import qualified Data.Set as Set
-- ********************************************************
-- Various helper functions 
-- not all need to be used
-- int_to_string 637287 -> "637287"
int_to_string :: Show a => a -> String
int_to_string n = show n

-- convert_list "3456789" -> [3,4,5,6,7,8,9]
convert_list :: String -> [Int]
convert_list = map (read . return) . concat . lines

-- Convert integer to list of digits
-- list_of_digits 3456789 -> [3,4,5,6,7,8,9]
list_of_digits :: Show a => a -> [Int]
list_of_digits n = (convert_list . int_to_string) n

-- Convert String to Integer
-- convert_to_int "12345" -> 12345
convert_to_int :: String -> Integer
convert_to_int n = read n :: Integer

-- Convert integer digit list to Integer
-- convert_digit_list [1,2,3,4] -> 1234
convert_digit_list :: Show a => [a] -> Integer
convert_digit_list lst = convert_to_int$concat$ map show lst
-- ************************************************************
-- Build list of multiples 
-- lst2_6 45 -> [90,135,180,225,270]
lst2_6 :: Num a => a -> [a]
lst2_6 x = [2*x,3*x,4*x,5*x,6*x]

-- Takes a list of integers, converts each
-- integer to a String, and then sorts each individual string
-- sort_lst [90,135,180,225,270] -> ["09","135","018","225","027"]
sort_lst :: Show a => [a] -> [[Char]]
sort_lst ls = map sort$map int_to_string ls

-- Tests if all elements in a list are equal
-- all_same ["09","135","018","225","027"] -> False
-- all_same ["234","234"] -> True
all_same :: (Eq a) => [a] -> Bool
all_same xs = and $ map (== head xs) (tail xs)

-- Filter the answer 
answer :: Integer
answer = head[n| n<-[100000..1000000], (all_same.sort_lst.lst2_6) n]

main :: IO ()
main = do  
    (putStrLn.show) answer


-- Congratulations, the answer you gave to problem 52 is correct.

-- You are the 63477th person to have solved this problem.

