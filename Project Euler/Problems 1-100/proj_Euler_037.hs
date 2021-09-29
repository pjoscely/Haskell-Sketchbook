{-
-- Project Euler
-- https://projecteuler.net/
'''
Truncatable primes
 
Problem 37
The number 3797 has an interesting property. Being prime itself, 
it is possible to continuously remove digits from left to right, 
and remain prime at each stage: 3797, 797, 97, and 7. 
Similarly we can work from right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable 
from left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
'''
-- ***************************************************************************************
-- Python solution first

def isPrime2(n):
    if n==2 or n==3: return True
    if n%2==0 or n<2: return False
    for i in range(3, int(n**0.5)+1, 2):   # only odd numbers
        if n%i==0:
            return False    
    return True

def ltrunc(n):
    s = str(n)
    for i in range(len(s)):
        v = int(s[i:])
        if isPrime2(v) == False:
            return False
    return True
    
def rtrunc(n):
    s = (str(n))
    for i in range(len(s), 0, -1):
        v = int(s[0:i])
        if isPrime2(v) == False:
            return False
    return True
trunc = []
for i in range(10, 10**6):
    if ("0" not in str(i) and ltrunc(i) and rtrunc(i)):
        trunc.append(i)

print(trunc)
s = 0
for n in trunc:
    s+=n
print(s)     

'''
Congratulations, the answer you gave to problem 37 is correct.

You are the 71723rd person to have solved this problem.
'''
-}
-- **************************************************************************************
-- Haskell Solution ~ 
-- Brute Force rather slow, but it is what it is 

-- computes the ceiling of the square root 
-- isqrt 17 = 5
isqrt :: (Integral b, Integral a) => a -> b
isqrt x = ceiling (sqrt(fromIntegral x))

-- Tests if a number is prime by division up to isqrt k
-- Special case included for k = 2
isPrime :: Integral a => a -> Bool
isPrime 2 = True
isPrime k = if k > 2 then null [ x | x <- [2..isqrt k], k `mod` x == 0] else False

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

-- Checks if a list of digits is left truncatable
-- ltrunc[3,7,9,7] ->True 3797, 797,97, 7 are all primes
ltrunc :: Show a => [a] -> Bool
ltrunc [] = True
ltrunc (x:xs) = (isPrime$convert_digit_list (x:xs)) && ltrunc (xs)

-- Checks if a list of digits is right truncatable
-- ltrunc[3,7,9,7] ->True 3797, 379, 37, 3  are all primes
--ltrunc :: Show a => [a] -> Bool
rtrunc :: Show a => [a] -> Bool
rtrunc [] = True
rtrunc (x:xs) = (isPrime$convert_digit_list (x:xs)) && rtrunc (init (x:xs))

-- Filter and sum 
-- [23,37,53,73,313,317,373,797,3137,3797,739397]
total :: Integer
total = sum[n| n<-[23..999999], let lst =list_of_digits n, ltrunc lst,  rtrunc lst]

-- main -> 748317
-- (72.81 secs, 52,564,851,848 bytes)
main :: IO ()
main = do  
    putStrLn$show total







