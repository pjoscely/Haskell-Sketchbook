{-

Project Euler
https://projecteuler.net/

Prime permutations
 
Problem 49
The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, 
is unusual in two ways: 

(i) each of the three terms are prime, and, 
(ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, 
exhibiting this property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this sequence?
-}


import Data.List
import qualified Data.Set as Set
-- ********************************************************
-- Various helper functions 
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

-- computes the ceiling of the square root 
-- isqrt 17 = 5
isqrt :: (Integral b, Integral a) => a -> b
isqrt x = ceiling (sqrt(fromIntegral x))

-- Tests if a number is prime by division up to isqrt k
-- Special case included for k = 2
isPrime :: Integral a => a -> Bool
isPrime 2 = True
isPrime k = if k > 2 then null [ x | x <- [2..isqrt k], k `mod` x == 0] else False

-- This returns all possible ways of inserting a new element into a list.
-- interleave 1 [2,3,4] -> [[1,2,3,4],[2,1,3,4],[2,3,1,4][2,3,4,1]]
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- This returns all permutations of a list, which are given by all possible
-- reorderings of the elements.
-- perms [1,2,3] -> [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- Combinations of size k from a list
-- combinations 3 [7993,9397,9739,9973] ->
-- [[7993,9397,9739],[7993,9397,9973],[7993,9739,9973],[9397,9739,9973]]
combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k==).length) $ subsequences ns

-- Tests if a 3 element list is arithmetic
-- is_arth [1487, 4817, 8147] -> True	
is_arth :: (Eq a, Num a) => [a] -> Bool
is_arth lst = (lst!!1 - lst!!0) == (lst!!2 - lst!!1)

-- Generate list of possible primes
primes :: [Integer]
primes = [p| p<-[1009..9973], isPrime p]

-- Cast the prime list of as a set 
set_primes :: Set.Set Integer
set_primes = Set.fromList primes

-- Generate candidate lists of length 3 or more 
-- These are increasing lists of permutations of a single prime, 
-- which are all primes and consist of 3 or more primes. For  
-- example the first 3 runs are: 
--  [1013,1031,1103,1301,3011],[1019,1091,1109,1901,9011],
--  [1021,1201,2011]]


runs :: [[Integer]]
runs  = [t'| p<- primes, let s = Set.fromList$map convert_digit_list$ perms$list_of_digits p,
        let t = Set.intersection s set_primes, Set.size t >= 3, let t' = Set.toList t]

-- This filters runs looking at all combinations of size 3 
-- that are arithmetic
-- [2699,2969,6299,9629] is easily found 
ans  = [r|r<-runs, let vals = map is_arth$combinations 3 r, elem True vals] 

-- main is included for completeness
main :: IO ()
main = do  
    (putStrLn.show) 296962999629

--  = 296962999629 

-- Congratulations, the answer you gave to problem 49 is correct.
-- You are the 56194th person to have solved this problem.
