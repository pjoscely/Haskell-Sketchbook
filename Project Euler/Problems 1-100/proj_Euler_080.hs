-- Project Euler
-- https://projecteuler.net/
-- """
-- Problem 80
-- It is well known that if the square root of a natural number is not an integer, 
-- then it is irrational. The decimal expansion of such square roots is infinite 
-- without any repeating pattern at all.

-- The square root of two is 1.41421356237309504880..., and the digital sum 
-- of the first one hundred decimal digits is 475.

-- For the first one hundred natural numbers, find the total of the digital 
-- sums of the first one hundred decimal digits for all the irrational square roots.
-- **************************************************************************************
-- Python Solution to "prime the pump":
-- """
-- from decimal import * #Compute big square roots 
-- import math
-- # Check for irrational roots
-- def is_irrational(num):
--     int_sqrt = int(math.sqrt(num))
--     if int_sqrt**2 != num:
--         return True
--     else:
--         return False

-- getcontext().prec = 102 #added precision to avoid round off

-- #Used to generate a list roots square
-- def print_dec_sqrts():
--     for i in range (1,101):
--         if(is_irrational(i)):
 --            print(Decimal(i).sqrt())
            
-- #Digit sum function         
-- def digit_sum(num):
--     sum = 0
--     num_str = str(num)
--     for item in num_str:
--         sum+=int(item)
--     return sum

-- #read the above from a text file
-- with open('p_080.txt') as f:
--     content = f.readlines()
-- # you may also want to remove whitespace characters like `\n` at the end of each line
-- content = [x.strip() for x in content] 

-- sum = 0
-- for item in content:
--     item = item[0:100]#extract first 100 digits
--     sum+=digit_sum(item)
-- print(sum) #print sum

-- '''
-- Congratulations, the answer you gave to problem 80 is correct.

-- You are the 18747th person to have solved this problem.

-- This problem had a difficulty rating of 20%. 
-- The highest difficulty rating you had previously solved was 5%. 
-- This is a new record. Well done!
-- '''
-- **************************************************************************************
-- Haskell Solution


