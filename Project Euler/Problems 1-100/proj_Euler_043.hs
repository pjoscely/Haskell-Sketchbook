{-
-- Project Euler
-- https://projecteuler.net/
Sub-string divisibility
  Show HTML problem content  
Problem 43
The number, 1406357289, is a 0 to 9 pandigital number because it is made 
up of each of the digits 0 to 9 in some order, but it also has a rather 
interesting sub-string divisibility property.

Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:

d2d3d4=406 is divisible by 2
d3d4d5=063 is divisible by 3
d4d5d6=635 is divisible by 5
d5d6d7=357 is divisible by 7
d6d7d8=572 is divisible by 11
d7d8d9=728 is divisible by 13
d8d9d10=289 is divisible by 17
Find the sum of all 0 to 9 pandigital numbers with this property.
-}

import Data.List
import qualified Data.Set as Set
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

-- Finds the equivalent decimal value of a three digit list
-- three_digit_val [0,6,3] -> 63
three_digit_val :: Num a => [a] -> a
three_digit_val lst = (lst!!0)*100+(lst!!1)*10+lst!!2

-- Helper Function 
-- is_divisible [0,6,3] 7 -> True
is_divisible :: Integral a => [a] -> a -> Bool
is_divisible lst k = (three_digit_val lst) `mod` k == 0 

-- This returns all possible ways of inserting a new element into a list.
-- interleave 1 [2,3,4] -> [[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]]
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- This returns all permutations of a list, which are given by all possible
-- reorderings of the elements.
-- perms [1,2,3] -> [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- Checks if n is 0 to 9 pandigital
-- is_pan 1406357289 True
is_pan :: Show a => a -> Bool
is_pan n = if (Set.size (Set.fromList lst) == 10) then True else False
         where lst = list_of_digits n 

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)


-- Filter using permutations the possible 0 to 9 pandigitials
p_lst :: [[Integer]]
p_lst = [p|p<-perms [0,1,2,3,4,5,6,7,8,9],head p /= 0, p!!5 == 5, p!!3 `mod` 2 == 0]


-- Tests the substring divisibilty condition
has_prop :: Show a => a -> Bool
has_prop n = is_divisible (slice 2 4 lst) 3 && is_divisible (slice 4 6 lst) 7 &&
             is_divisible (slice 5 7 lst) 11 && is_divisible (slice 6 8 lst) 13 &&
             is_divisible (slice 7 9 lst) 17
             where lst = list_of_digits n 

-- Generate the six solution lists
-- answer_lst 
-- (18.03 secs, 18,117,619,376 bytes)
-- [[1,4,0,6,3,5,7,2,8,9],[1,4,6,0,3,5,7,2,8,9],
-- [4,1,0,6,3,5,7,2,8,9],[4,1,6,0,3,5,7,2,8,9],
-- [1,4,3,0,9,5,2,8,6,7],[4,1,3,0,9,5,2,8,6,7]]
-- answer_lst :: [[Integer]]
answer_lst = [p|p<-p_lst, (is_pan.convert_digit_list) p ,has_prop$convert_digit_list p] 

-- sum answers as Integers
answer :: Integer
answer = (sum.map convert_digit_list) [[1,4,0,6,3,5,7,2,8,9],[1,4,6,0,3,5,7,2,8,9],
                                    [4,1,0,6,3,5,7,2,8,9],[4,1,6,0,3,5,7,2,8,9],
                                    [1,4,3,0,9,5,2,8,6,7],[4,1,3,0,9,5,2,8,6,7]]

--main -> 16695334890
main :: IO ()
main = do  
    (putStrLn.show) answer

-- Congratulations, the answer you gave to problem 43 is correct.

-- You are the 58055th person to have solved this problem.


