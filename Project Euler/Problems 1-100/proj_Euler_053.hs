{-

Project Euler

https://projecteuler.net/
Combinatoric selections
 
Problem 53
There are exactly ten ways of selecting three from five, 12345:

123, 124, 125, 134, 135, 145, 234, 235, 245, and 345


It is not until 23, that a value exceeds one-million: 
23 Choose 10 = 1144066

How many, not necessarily distinct, values of 
n Choose r for 1<= n<= 100 are greater than one-million?
-}
import Data.List

-- Computes factorial
fac :: (Eq p, Num p) => p -> p
fac 0 = 1
fac 1 = 1
fac n = n*fac (n-1)

-- Computes n Choose r
nCr :: Integral p => p -> p -> p
nCr n r = (fac n) `div` ((fac r)*fac (n-r))

-- Computes total nCr > 1,000,000
answer :: Integer
answer = sum[1| n <- [1..100], r<-[1..100], r<=n, (nCr n r )> 10^6]

-- main -> 4075
-- (0.86 secs, 276,518,096 bytes)
main :: IO ()
main = do  
    (putStrLn.show) answer 

-- Congratulations, the answer you gave to problem 53 is correct.

-- You are the 57320th person to have solved this problem.
