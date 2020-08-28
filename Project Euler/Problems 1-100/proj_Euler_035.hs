{-
Circular primes
https://projecteuler.net/problem=35
Problem 35
'''

The number, 197, is called a circular prime because all rotations of the digits: 
    197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 
    2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

How many circular primes are there below one million?

*********************************************************************************
Python solution was completed first
'''

set_primes = set()

def isPrime2(n):
    if n==2 or n==3: return True
    if n%2==0 or n<2: return False
    for i in range(3, int(n**0.5)+1, 2):   # only odd numbers
        if n%i==0:
            return False    
    return True

for n in range(2,1000000):
    if(isPrime2(n)):
        set_primes.add(n)

filter_primes = set() 
for n in set_primes:
      str_n = str(n)
      if ('0' not in  str_n) and ('2' not in  str_n) and ('4' not in  str_n) and 
      ('6' not in  str_n) and ('8' not in  str_n ):
          filter_primes.add(n)

filter_primes.add(2)

def is_circular(n):
    str_n = str(n)
    for i in range (1, len(str_n)):
            temp = str_n[i:]+str_n[0:i]
            if (int(temp) not in filter_primes):
                return False
    return True
       
ct = 0
for n in filter_primes:
    if(is_circular(n)):
        ct+=1
        print(n)
print(ct)

-}
-- ********************************************************
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


-- Rotates a list
-- rotate 2 [1,2,3] -> [3,1,2]
-- rotate 0 [1,2,3] -> [1,2,3]
-- rotate 3 [1,2,3] -> [1,2,3]
-- rotate 15  [1,2,3] -> [1,2,3]
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

-- Any two or more digit circular prime cannot contain the digits:
-- 2, 4, 5, 6, 8

is_valid n = not (2 `elem` l) && not (4 `elem` l) && not (6 `elem` l) && not (8 `elem` l) && not (5 `elem` l) 
             where l = list_of_digits n



