
{-
Arranged probability

https://projecteuler.net/problem=100

Problem 100

If a box contains twenty-one coloured discs, composed of fifteen blue discs and six red discs, 
and two discs were taken at random, it can be seen that the probability of taking two blue discs, 
P(BB) = (15/21)×(14/20) = 1/2.

The next such arrangement, for which there is exactly 50% chance of taking two blue discs at random, 
is a box containing eighty-five blue discs and thirty-five red discs.

By finding the first arrangement to contain over 10^12 = 1,000,000,000,000 discs in total, 
determine the number of blue discs that the box would contain.
****************************************************************************************************
Python Solution
Note:
(b/n)*(b-1)/(n-1)=1/2 is equivalent to:
2*b**2 - 2*b - n**2 + n = 0

https://www.alpertron.com.ar/QUAD.HTM
gives a recursive algorithm to generate solutions
'''

b = 15
n = 21
target = 10**12
 
while(n < target):
    xt = 3 * b + 2 * n - 2
    yt = 4 * b + 3 * n - 3
 
    b = xt
    n = yt
print(b,n)
#Congratulations, the answer you gave to problem 100 is correct.

#You are the 15421st person to have solved this problem.
-}
-- ***********************************************************************************
-- Haskell Solution
import Data.List
import Data.Maybe

-- Iteration functions for b and n
-- b is number of blue chips
-- n is total number of chips
fx :: Num a => a -> a -> a
fx b n = 3 * b + 2 * n - 2

fy :: Num a => a -> a -> a
fy b n = 4 * b + 3 * n - 3

-- Generate iteration pairs 
-- f(15,21) -> (85,120) -> (493,697)
f :: Num b => (b, b) -> (b, b)
f (b, n) = (fx b n, fy b n)

-- Lazy generation of list of iterates 
-- iterate f (15, 21) -> [(15,21),(85,120),(493,697),(2871,4060),(16731,23661),...,]
lst = iterate f (15, 21)

-- Find the first n value > 10^12
-- total -> 1070379110497
total :: Integer
total = head $filter (>10^12)$map snd lst

-- Find index of pair in lst where n = total
-- indx -> 14
indx :: Int
indx = fromJust$elemIndex total (map snd$ lst)

-- answer is the first coordinate at the indx
-- fst (756872327473,1070379110497) -> 756872327473
answer :: Integer
answer = fst$ iterate f (15, 21)!!indx

-- Display answer
--  main -> 756872327473
-- (0.04 secs, 156,464 bytes)
main :: IO ()
main = do  
    (putStrLn.show) answer

