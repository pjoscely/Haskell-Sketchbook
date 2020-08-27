{-
'''
Pandigital products
https://projecteuler.net/problem=32
Problem 32
We shall say that an n-digit number is pandigital if it makes use of all 
the digits 1 to n exactly once; for example, the 5-digit number, 
15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254, 
containing multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product 
identity can be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way 
so be sure to only include it once in your sum.

**********************************************************************************
Python Solution
'''
#Generates all permutations 
from itertools import permutations 

#Store unique solutions
products = set()

#Generate all permuations
perm_digits = [''.join(p) for p in permutations('123456789')] 

#For each permutation of '123456789'
#Break into three parts and check multiplicand/multiplier/product 
#e.g.18 297 5346
for item in perm_digits:
    for i in range(1, 8):
        for j in range(i+1, 9):
            multiplicand = int(item[0:i])
            multiplier = int(item[i:j])
            product = int(item[j:])
            if(multiplicand* multiplier== product):
                products.add(product)
                
sum = 0
for item in products:
    sum+=item
print(sum)
#45228
#Congratulations, the answer you gave to problem 32 is correct.

#You are the 69303rd person to have solved this problem.
-}
-- **********************************************************************************
-- Haskell Solution
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
list_to_num :: Show a => [a] -> Integer
list_to_num lst = convert_to_int$concat$ map show lst

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

-- Generate a list all permutations of 123456789
-- [[1,2,3,4,5,6,7,8,9],[2,1,3,4,5,6,7,8,9],[2,3,1,4,5,6,7,8,9]...
perm_digits = perms $ convert_list  "123456789"

-- 
-- slice 0 3 [1,2,3,4,5,6,7,8,9] -> [1,2,3,4]
-- slice 0 0  [1,2,3,4,5,6,7,8,9] -> [1]
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

-- Generate solution set of products, use the set feature to filter out repeats
-- [4396,5346,5796,6952,7254,7632,7852]
soln :: Set.Set Integer
soln = Set.fromList[prod| p<-perm_digits , i <-[0..6], j<- [i+1..7], 
           let multiplicand = list_to_num$slice 0 i p,
           let multiplier = list_to_num$slice (i+1) j p,
           let prod = list_to_num$slice (j+1) 8 p,
           multiplicand*multiplier == prod]

-- Compute sum of all possible products
answer :: Integer
answer = sum soln

-- Display answer 
-- 45228
main :: IO ()
main = do  
    (putStrLn.show) answer

