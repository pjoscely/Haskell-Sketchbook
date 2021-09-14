{-
-- Project Euler
-- https://projecteuler.net/
'''
Non-abundant sums
Problem 23
A perfect number is a number for which the sum of its proper divisors is exactly equal 
to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, 
which means that 28 is a perfect number.
A number n is called deficient if the sum of its proper divisors is less than n 
and it is called abundant if this sum exceeds n.
As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number 
that can be written as the sum of two abundant numbers is 24. By mathematical analysis, 
it can be shown that all integers greater than 28123 can be written as the sum of 
two abundant numbers. However, this upper limit cannot be reduced any further by analysis 
even though it is known that the greatest number that cannot be expressed as the sum 
of two abundant numbers is less than this limit.
Find the sum of all the positive integers which cannot be written as the sum of 
two abundant numbers.
'''
import time
import math
start = time.time()
#total number of divisiors 
def sum_of_divisors(num):
    sum = 0
    lim = math.sqrt(num)
    for i in range(1,int(lim)+1):
        if(num%i == 0):
            sum+=i
            sum+=num/i
            
    if(int(lim)**2 == num):
        sum -=lim
    
    return int(sum -num)
limit = 28214 #limit supplied by problem
abundant_list =[] #list of abundant numbers
for i in range(12,limit):
    if(sum_of_divisors(i) > i):
        abundant_list.append(i)
sum_list = set() #set of nums expressible as a sum of two abundants
#Build set of nums expressible as a sum of two abundants
for x in abundant_list:
    for y in abundant_list:
        if x + y < limit:
            sum_list.add(x + y)
#sum all nums not expressible as a sum of two abundants           
sum = 0
for i in range(1,limit):
    if i not in sum_list:
        sum+=i
print(sum)
end = time.time()
print("Time", end - start)
#4179871
#Time 10.721098184585571
#Congratulations, the answer you gave to problem 23 is correct.
#You are the 101686th person to have solved this problem. 
-}

-- ******************************************************************
-- Haskell Solution

import Data.List
import Data.IntSet (toList, fromList)

-- Test if a number is a perfect square
perfect_square :: Integral a => a -> Bool
perfect_square n = sq * sq == n
    where sq = floor $ sqrt $ (fromIntegral n::Double)

-- int_square_root 5 = 2 
-- int_square_root 4 = 2 
int_square_root :: (Integral b, Integral a) => a -> b
int_square_root n = floor $ sqrt $ (fromIntegral n::Double)

-- Helper function
-- sum_tuple_pair (1,2) ->3
sum_tuple_pair :: Num a => (a, a) -> a
sum_tuple_pair (x,y)= x+y

-- Create raw tuple divisor pairs 
-- tuple_pairs 24 -> [(1,24),(2,12),(3,8),(4,6)]
tuple_pairs :: Integral b => b -> [(b, b)]
tuple_pairs n = [(c,n`div`c)|c<-[1..floor $ sqrt $ (fromIntegral n::Double)],n`rem`c == 0]

-- Correct by subtracting the divisor = n
sum_tuple_pairs :: (Integral a, Num b) => a -> b
sum_tuple_pairs n = fromIntegral((sum$map sum_tuple_pair$tuple_pairs n)  -  n)

-- final sum of proper divisors
-- sum_of_div 284 -> 220
sum_of_div :: (Integral a, Integral p) => a -> p
sum_of_div n = if perfect_square n then  sum_tuple_pairs n - int_square_root n else sum_tuple_pairs n

-- nub' removes duplicates from a list, it is much faster than the List function nub
answer = sum [1..20161] - (sum $ nub' $ allSums abun)
  where nub' = toList . fromList
        abun = filter (\x -> x < sum_of_div x) [12..20149]
        allSums xs = [(x + y) | x <- xs, y <- xs, y >= x, (x + y) <= 20161]

main :: IO ()
main = do  
    putStrLn$show answer
     
