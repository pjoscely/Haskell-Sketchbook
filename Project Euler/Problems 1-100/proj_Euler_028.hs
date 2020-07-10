{-

-- Project Euler
-- https://projecteuler.net/

Number spiral diagonals
 
Problem 28
Starting with the number 1 and moving to the right in a clockwise direction a 
7 by 7 spiral is formed as follows:

43 44 45 46 47 48 49
42 21 22 23 24 25 26
41 20  7  8  9 10 27
40 19  6  1  2 11 28
39 18  5  4  3 12 29
38 17 16 15 14 13 30
37 36 35 34 33 32 31

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
-}

-- The spiral, is also called Ulam's Spiral 
-- https://en.wikipedia.org/wiki/Ulam_spiral

-- We create four sums corresponding to the four diagonal directions
-- emanating from the center. 
-- Formulas for the diagonals come from https://oeis.org

size :: Integer
size = 1001`div`2

-- [1,9,25,49,81,121,169,225,289,361,441...]
sum_1 :: Integer
sum_1 = sum[(2*n+1)^2| n<-[0..size]]

-- [3,13,31,57,91,133,183,241,307,381...]
sum_2 :: Integer
sum_2 = sum[4*n^2 - 10*n + 7| n<-[2..size+1]]

-- [7,21,43,73,111,157,211,273,343,421...]
sum_3 :: Integer
sum_3 =  sum[4*n^2 - 6*n + 3| n<-[2..size+1]]

-- [5,17,37,65,101,145,197,257,325,401...]
sum_4 :: Integer
sum_4 = sum [4*n^2 + 1 | n<-[1..size]]

-- Compute total
answer :: Integer
answer = sum_1+sum_2+sum_3+sum_4

--  main -> 669171001
-- (0.01 secs, 1,866,712 bytes)
main :: IO ()
main = do  
    putStrLn$show answer

-- Congratulations, the answer you gave to problem 28 is correct.

-- You are the 105915th person to have solved this problem.

