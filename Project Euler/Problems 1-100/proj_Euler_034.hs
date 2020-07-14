{-
'''
-- Project Euler
-- https://projecteuler.net/

Digit factorials
 
Problem 34
145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial 
of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.
'''
-- **************************************************************************************
-- Python Solution to "prime the pump":
def fac(n):
    if n == 0 or n == 1:
        return 1
    else:
        return n*fac(n-1)
    
#Test for curious number
def is_sum(num):
    sum = 0
    str_num = str(num)
    for c in str_num:
        sum+=fac(int(c))
    if sum == num:
        return True
    else:
        return False
    
#Explore in range below 
for i in range(3,10000000):
    if is_sum(i):
        print(i)
'''
Congratulations, the answer you gave to problem 34 is correct.

You are the 92221st person to have solved this problem.
'''
-}
-- **************************************************************************************
-- Haskell Solution
-- Factorial function
fac :: (Eq p, Num p) => p -> p
fac n = if n == 0 || n == 1 then 1 else n*fac(n-1)

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

-- Compute sum of factorial of digits
-- sum_fac 145 = 1! + 4! + 5! = 1 + 24 + 120 = 145
sum_fac :: Show a => a -> Int
sum_fac n = sum$map fac$list_of_digits n

-- Form list of solutions
solutions :: [Int]
solutions= [n|n<-[3..100000],sum_fac n == n]

 -- main (4.22 secs, 3,269,338,720 bytes)
 -- [145,40585]
main :: IO ()
main = do  
    putStrLn$show solutions


