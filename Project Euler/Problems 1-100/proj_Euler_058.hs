{-

Project Euler
https://projecteuler.net/

Spiral primes
   
Problem 58
Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.

37 36 35 34 33 32 31
38 17 16 15 14 13 30
39 18  5  4  3 12 29
40 19  6  1  2 11 28
41 20  7  8  9 10 27
42 21 22 23 24 25 26
43 44 45 46 47 48 49

It is interesting to note that the odd squares lie along the bottom right diagonal, 
but what is more interesting is that 8 out of the 13 numbers lying 
along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.

If one complete new layer is wrapped around the spiral above, 
a square spiral with side length 9 will be formed. If this process is continued, 
what is the side length of the square spiral for which the ratio of primes 
along both diagonals first falls below 10%?

***********************************************************************************
Python solution

def isPrime(n):
    if n==2 or n==3: return True
    if n%2==0 or n<2: return False
    for i in range(3, int(n**0.5)+1, 2):   # only odd numbers
        if n%i==0:
            return False    
    return True

#Count primes on the diagonals 
#Break when number of primes is
#less than the number on the diagonal

def solution():
  p = 0
  m = 3
  while (True):
    p+= isPrime(m**2 -     m + 1)
    p+= isPrime(m**2 - 2 * m + 2)
    p+= isPrime(m**2 - 3 * m + 3)
    if (10 * p < 2 * m - 1):
      return m
    m+=2 
    
print(solution()) 
26241
******************************************************************
-}
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
-- size :: Integer
-- size = 7`div`2

-- computes the ceiling of the square root 
-- isqrt 17 = 5
isqrt :: (Integral b, Integral a) => a -> b
isqrt x = ceiling (sqrt(fromIntegral x))

-- Tests if a number is prime by division up to isqrt k
-- Special case included for k = 2
isPrime :: Integral a => a -> Bool
isPrime 2 = True
isPrime k = if k > 2 then null [ x | x <- [2..isqrt k], k `mod` x == 0] else False

{-

sum_2 size = Set.fromList [(n,4*n^2 - 10*n + 7 )| n<-[2..size+1], odd n == True, isPrime (4*n^2 - 10*n + 7)]

-- [7,21,43,73,111,157,211,273,343,421...]

sum_3 size = Set.fromList [(n,4*n^2 - 6*n + 3)| n<-[2..size+1], isPrime (4*n^2 - 6*n + 3)]

-- [5,17,37,65,101,145,197,257,325,401...]

sum_4 size = Set.fromList [ (n,4*n^2 + 1)| n<-[1..size], isPrime (4*n^2 + 1)]
-}
whileLoop a =
            if(a `mod `97 /=0)
                then whileLoop (a+2)
                else a


count l =
    let accumulate acc el = el + acc
    in foldl' accumulate 0 l


sum_2 size = sum[1| n<-[3,5..size], isPrime (4*n^2 - 10*n + 7)]
