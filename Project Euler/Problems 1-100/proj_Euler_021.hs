-- '''
-- Amicable numbers
 
-- Problem 21
-- Let d(n) be defined as the sum of proper divisors of n 
-- (numbers less than n which divide evenly into n).
-- If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair 
-- and each of a and b are called amicable numbers.

-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; 
-- therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

-- Evaluate the sum of all the amicable numbers under 10000.
-- ***************************************************************************************
-- Python solution first
-- '''
-- import math
-- #Compute sum of divisiors 
-- def sum_of_divisors(num):
--   sum = 0
--   lim = math.sqrt(num)
--     for i in range(1,int(lim)+1):
--         if(num%i == 0):
--             sum+=i
--             sum+=num/i           
--     if(int(lim)**2 == num):
--        sum -=lim
-- # Filter and print the amicable pairs
-- sum = 0
-- for i in range(220,10000):
--     d = sum_of_divisors(i)
--     if(i == sum_of_divisors(d) and d!=i):
--         sum+=(i+d)
--         print(i,d)
-- #Divie by two, since pairs are counted twice above
-- print(int(sum/2))
-- 220 284
-- 284 220
-- 1184 1210
-- 1210 1184
-- 2620 2924
-- 2924 2620
-- 5020 5564
-- 5564 5020
-- 6232 6368
-- 6368 6232
-- 31626
    
-- #Congratulations, the answer you gave to problem 21 is correct.
-- #You are the 143557th person to have solved this problem.

-- ***************************************************************************************
-- Haskell solution

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

-- Create amicable number pairs less than 100000
-- [(220,284),(284,220),(1184,1210),(1210,1184),(2620,2924),(2924,2620),
-- (5020,5564),(5564,5020),(6232,6368),(6368,6232)]
-- *** Note there are duplicates ***
amicable_number_pairs :: [(Integer, Integer)]
amicable_number_pairs = [(a,d_a)|a<-[220..9999],let d_a = sum_of_div a, let d_b = sum_of_div d_a, a == d_b, a/=d_a]

-- answer -> 31626
-- (0.98 secs, 396,881,224 bytes)
-- Divide by 2 to account for duplicates 
answer :: Integer
answer = (sum $ map sum_tuple_pair amicable_number_pairs) `div` 2

-- main -> 31626
-- (0.98 secs, 396,881,248 bytes)
main :: IO ()
main = do  
    putStrLn$show answer




