{-
-- Project Euler
-- https://projecteuler.net/
'''
Pandigital multiples
 
Problem 38
Take the number 192 and multiply it by each of 1, 2, and 3:

192 × 1 = 192
192 × 2 = 384
192 × 3 = 576
By concatenating each product we get the 1 to 9 pandigital, 192384576. 
We will call 192384576 the concatenated product of 192 and (1,2,3)

The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, 
giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

What is the largest 1 to 9 pandigital 9-digit number that can be formed as the 
concatenated product of an integer with (1,2, ... , n) where n > 1?
**************************************************************************************
-- Python Solution to "prime the pump":
'''
#Checks if n is pandigital 9-digit number
def is_pan(n):
    d = set()
    if(n > 987654321):
        return False
    str_n = str(n)
    for c in str_n:
        d.add(c)
    if len(d) == 9 and "0" not in d:
        return True
    else:
        return False
#Given n this builds longest concatenation <= length 9 
def build(n):
    s = str(n)
    temp = s
    i = 2
    while(True):
        s+=str(n*i)
        if(len(s)> 9):
            return int(temp)
        else:
            temp = s
            i+=1  

#Use the example supplied 
max = 918273645

#Find largest pandigital 9-digit number that can be formed as described
#in the problem
for i in range (1, 10000):
    n= build(i)
    if(is_pan(n) and n > max):
        max = n
print(max)

'''
932718654
Congratulations, the answer you gave to problem 38 is correct.

You are the 61289th person to have solved this problem.
'''
-}
-- ***************************************************************************************
-- Haskell solution
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

-- Checks if n is 1 to 9 pandigital
-- is_pan 987654321 -> True
-- is_pan 122309876 -> False
is_pan :: Show a => a -> Bool
is_pan n = if (not(elem 0 lst)) && (Set.size (Set.fromList lst) == 9) then True else False
         where lst = list_of_digits n 

-- Adapted from Proj Euler 
-- Clever way to filter n to be checked for being 1 t0 9 pandigitial
answer :: Integer
answer = maximum [n*((10^5)+2)| n <- [999..9999], is_pan(n*((10^5)+2))]

-- main -> 932718654
-- (0.12 secs, 225,664,864 bytes)
main :: IO ()
main = do  
    putStrLn$show answer



