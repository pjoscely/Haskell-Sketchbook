{-
Totient maximum

Problem 69
Euler's Totient function, φ(n) [sometimes called the phi function], is used to determine the number of numbers less than n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.

n	Relatively Prime	φ(n)	n/φ(n)
2	1	1	2
3	1,2	2	1.5
4	1,3	2	2
5	1,2,3,4	4	1.25
6	1,5	2	3
7	1,2,3,4,5,6	6	1.1666...
8	1,3,5,7	4	2
9	1,2,4,5,7,8	6	1.5
10	1,3,7,9	4	2.5
It can be seen that n=6 produces a maximum n/φ(n) for n ≤ 10.

Find the value of n ≤ 1,000,000 for which n/φ(n) is a maximum.
-}
-- https://rosettacode.org/wiki/Totient_function#Haskell
{-# LANGUAGE BangPatterns #-}
 
import Control.Monad (when)
import Data.Bool (bool)

totient
  :: (Integral a)
  => a -> a
totient n
  | n == 0 = 1 -- by definition phi(0) = 1
  | n < 0 = totient (-n) -- phi(-n) is taken to be equal to phi(n)
  | otherwise = loop n n 2 --
  where
    loop !m !tot !i
      | i * i > m = bool tot (tot - (tot `div` m)) (1 < m)
      | m `mod` i == 0 = loop m_ tot_ i_
      | otherwise = loop m tot i_
      where
        i_
          | i == 2 = 3
          | otherwise = 2 + i
        m_ = nextM m
        tot_ = tot - (tot `div` i)
        nextM !x
          | x `mod` i == 0 = nextM $ x `div` i
          | otherwise = x

-- Helper fucntion to divide tuples 
-- div_tuple (56,5) -> 11.2
div_tuple :: (Fractional a1, Integral a2, Integral a3) => (a2, a3) -> a1
div_tuple (a,b) = fromIntegral a / fromIntegral b

-- list of totient values 2 to 10^6
t :: [Integer]
t = [fromIntegral$totient n| n <- [2..1000000]]

-- list starting 2 to 10^6
s :: [Integer]
s = map fromIntegral [2..1000000]

-- list of tuples (n, totient n)
a :: [(Integer, Integer)]
a = zip s t

-- list of ratios n/totient n
r :: [Double]
r = map div_tuple a

-- answer (5.539388020833333,510510)
-- (203.34 secs, 81,524,169,816 bytes)
answer :: (Double, Integer)
answer = maximum$zip r s

-- Congratulations, the answer you gave to problem 69 is correct.

-- You are the 33461st person to have solved this problem.
