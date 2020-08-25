
{-
Convergents of e

https://projecteuler.net/problem=65

Problem 65

What is most surprising is that the important mathematical constant, e
has continued fraction expansion
= [2;1,2,1,1,4,1,1,6,1,...,1,2k,...]

The first ten terms in the sequence of convergent numerators for e are:

1 2
2 3
3 8
4 11
5 19
6 87
7 106
8 193
9 1264
10 1457

The sum of digits in the numerator of the 10th convergent is 1+4+5+7 = 17

Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e.

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

-- https://oeis.org/A113873/b113873.txt gives the 100th term
-- 6963524437876961749120273824619538346438023188214475670667
ans = sum$list_of_digits 6963524437876961749120273824619538346438023188214475670667

-- main -> 272
-- (0.00 secs, 349,776 bytes)
main :: IO ()
main = do  
    (putStrLn.show) ans

-- Congratulations, the answer you gave to problem 65 is correct.

-- You are the 28785th person to have solved this problem.
