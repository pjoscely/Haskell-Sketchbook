{-
Project Euler

https://projecteuler.net/
Combinatoric selections

Square root convergents
 
Problem 57

It is possible to show that the square root of two can be expressed 
as an infinite continued fraction.
3/2 -> 7/5 -> 17/12 -> 41/29 ->...

In the first one-thousand expansions, how many fractions contain a 
numerator with more digits than the denominator?

-}
import Data.List
import qualified Data.Set as Set
-- ********************************************************
-- Various helper functions that might be useful
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

-- https://oeis.org/A000129
-- Computes denominator in continued fraction 
-- expansion of sqrt(2)
-- Pell numbers: 0, 1, 2, 5, 12, 29, 70, 169, 408, 985, 2378, 5741, 13860,...
memoized_den :: Int -> Integer
memoized_den = (map den [0 ..] !!)
   where den 0 = 0
         den 1 = 1
         den n = 2*memoized_den (n-1) + memoized_den (n-2)

-- https://oeis.org/A001333
-- Computes numerator in continued fraction 
-- expansion of sqrt(2)
-- 	1, 1, 3, 7, 17, 41, 99, 239, 577, 1393, 3363, 8119, 19601,...
-- num 4 -> 17
num :: Int -> Integer
num n = memoized_den n - memoized_den (n-1)

-- Computes number of digits in a non-negative integer 
-- length_num 22345 -> 5
length_num :: Show a => a -> Int
length_num n = (length.int_to_string) n

-- Filter answer
-- (length_num.num) n > (length_num.memoized_den) (n-1)
-- Inludes offset (n-1) to align sequences
answer :: Integer
answer = sum[1|n<- [1..1000], (length_num.num) n > (length_num.memoized_den) (n-1)]

-- main -> 153

-- (0.06 secs, 23,752,488 bytes)
main :: IO ()
main = do  
    (putStrLn.show) answer

-- Congratulations, the answer you gave to problem 57 is correct.
-- You are the 40095th person to have solved this problem.


