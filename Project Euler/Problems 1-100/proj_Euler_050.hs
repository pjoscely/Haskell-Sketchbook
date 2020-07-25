{-
Project Euler
https://projecteuler.net/
Consecutive prime sum
  
Problem 50
The prime 41, can be written as the sum of six consecutive primes:

41 = 2 + 3 + 5 + 7 + 11 + 13
This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?
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

-- Generate list of primes less than a million
-- Should read primes from an external text file 
primes :: [Integer]
primes = [p| p<-[2..999999], isPrime p]

-- Cast the prime list of as a set 
set_primes :: Set.Set Integer
set_primes = Set.fromList primes

-- Slice helper function
-- slice 1 5 [0,1,2,3,4,5,6] -> [1,2,3,4,5]
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

-- Helper fucntion to search the second item of a 
-- list of tuples 
-- search 103 [(103,304151),(107,317711)] -> [304151]
search :: Eq a => a -> [(a, b)] -> [b]
search x = map snd . filter ((==x).fst)

-- Build sums of consecutive primes which are primes 
-- the first number gives the number of consecutive primes 
-- summed to produce the 2nd enrty in the tuple is the prime
-- list_pairs -> [(2,5),(4,17),(6,41),(12,197),(14,281)...
list_pairs :: [(Int, Integer)]
list_pairs = [ ((m-n +1), s)| n<-[0..1500], m<- [n+1..n+550], let s = sum$slice n m primes, Set.member s set_primes] 

-- Find the length of the longest consecutive prime sum
max_consec :: Int
max_consec = maximum$map fst list_pairs

-- Use the helper search function to find the 
-- prime which is the longest consecutive sum of primes
answer :: [Integer]
answer = search max_consec list_pairs

-- main-> [997651]
-- (63.82 secs, 45,912,945,568 bytes)
main :: IO ()
main = do  
    (putStrLn.show) answer

-- Congratulations, the answer you gave to problem 50 is correct.
-- You are the 60756th person to have solved this problem.




