{-
Reciprocal cycles
 
Problem 26
A unit fraction contains 1 in the numerator. 
The decimal representation of the unit fractions with 
denominators 2 to 10 are given:

1/2	= 	0.5
1/3	= 	0.(3)
1/4	= 	0.25
1/5	= 	0.2
1/6	= 	0.1(6)
1/7	= 	0.(142857)
1/8	= 	0.125
1/9	= 	0.(1)
1/10	= 	0.1
Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. 
It can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle 
in its decimal fraction part.
-}

-- computes the ceiling of the square root 
-- isqrt 17 = 5
isqrt :: (Integral b, Integral a) => a -> b
isqrt x = ceiling (sqrt(fromIntegral x))

-- Tests if a number is prime by division up to isqrt k
-- Special case included for k = 2
isPrime :: Integral a => a -> Bool
isPrime 2 = True
isPrime k = if k > 2 then null [ x | x <- [2..isqrt k], k `mod` x == 0] else False

-- Create list of primes less than 1000
-- [3,5,7,11,13, ... ,
lst_p :: [Integer]
lst_p =[p|p<- [7..999], isPrime p]

-- Given a modulo p and exponent k compute 10^k `mod` p
-- ord 13 6 -> 1
ord :: (Integral a, Integral b) => a -> b -> a
ord p k = 10^k `mod` p

-- Key idea: Find the order of 10 in N/pN
-- The length of the repetend (period of the repeating decimal)
-- of 1/p is equal to the order of 10 modulo p.

-- Compute the order of 10 in N/pN
-- order 997 -> 166
-- order 13 -> 6
order :: (Integral a1, Integral a2) => a1 -> a2
order p = head [k| k<-[1..1000], ord p k == 1] 

-- Compute maximum order 
answer :: Integer
answer = maximum[order p| p<- lst_p] +1 

-- main -> 983
-- (0.18 secs, 127,832,208 bytes)
main :: IO ()
main = do  
    (putStrLn.show) answer




