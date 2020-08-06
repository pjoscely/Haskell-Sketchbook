{-

Project Euler
https://projecteuler.net/

Goldbach's other conjecture
  
Problem 46
It was proposed by Christian Goldbach that every odd composite number 
can be written as the sum of a prime and twice a square.

9 = 7 + 2×1^2
15 = 7 + 2×2^2
21 = 3 + 2×3^2
25 = 7 + 2×3^2
27 = 19 + 2×2^2
33 = 31 + 2×1^2

It turns out that the conjecture was false.
What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
-}
import Data.List
import qualified Data.Set as Set
-- computes the ceiling of the square root 
-- isqrt 17 = 5
isqrt :: (Integral b, Integral a) => a -> b
isqrt x = ceiling (sqrt(fromIntegral x))

-- Tests if a number is prime by division up to isqrt k
-- Special case included for k = 2
isPrime :: Integral a => a -> Bool
isPrime 2 = True
isPrime k = if k > 2 then null [ x | x <- [2..isqrt k], k `mod` x == 0] else False

-- Generate tentative list of primes
primes :: [Integer]
primes = [p| p<-[2..10001], isPrime p]

-- Generate tentative list of squares
sqs :: [Integer]
sqs = [x^2| x<-[1..1000]]

-- Generate tentative list of odd composites
odd_comp :: [Integer]
odd_comp =  [n|n<-[9,11..10001], (not.isPrime) n]

-- Form set of primes + 2*square
gold :: Set.Set Integer
gold = Set.fromList[g|p<-primes, s<-sqs, let g = p+2*s]

-- Filter answer 
answer :: Integer
answer = head[n| n<-odd_comp, Set.member n gold == False]

-- main -> 5777
-- (2.86 secs, 1,273,456,464 bytes)
main :: IO ()
main = do  
    (putStrLn.show) answer

-- Congratulations, the answer you gave to problem 46 is correct.
-- You are the 59837th person to have solved this problem.
