
{-
Project Euler
https://projecteuler.net/

Quadratic primes
 
Problem 27
Euler discovered the remarkable quadratic formula:

n^2+n+41
It turns out that the formula will produce 40 primes for the consecutive integer values 0≤n≤39. 
However, when n=40,40^2+40+41=40(40+1)+41 is divisible by 41, and certainly when n=41,
41^2+41+41 is clearly divisible by 41.

The incredible formula n^2−79*n+1601 was discovered, which produces 80 primes 
for the consecutive values 0≤n≤79. 
The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

n^2+a*n+b, where |a|<1000 and |b|≤1000

where |n| is the modulus/absolute value of n
e.g. |11|=11 and |−4|=4
Find the product of the coefficients, a and b, for the quadratic expression that produces 
the maximum number of primes for consecutive values of n, starting with n=0.

-- **************************************************************************************
-- Python Solution to "prime the pump":

import math
def is_prime(n):
    if n < 2:
        return False
    if n % 2 == 0 and n > 2: 
        return False
    for i in range(3, int(math.sqrt(n)) + 1, 2):
        if n % i == 0:
            return False
    return True

list_b =[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 
         61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 
         137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 
         211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 
         293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 
         383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 
         463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 
         569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 
         647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 
         743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 
         839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 
         941, 947, 953, 967, 971, 977, 983, 991, 997]

max_len = 0
best_a = 0
best_b = 0
for a in range(-999, 1000):
    for b in list_b :
        n = 0
        run = 0
        while(is_prime(n**2+a*n+b)):
            n+=1
            run+=1
            if(run > max_len):
                max_len = run
                best_a = a
                best_b = b
print(max_len,best_a,best_b)
'''
71 -61 971
Congratulations, the answer you gave to problem 27 is correct.

You are the 85169th person to have solved this problem.
 '''               
-}

-- **************************************************************************************
-- Haskell Solution 
-- v 1.0 needs much work
import Data.List
-- the mutiplier b must be prime in n^2+a*n+b since n starts with 0
-- in n^2+a*n+b, this reduces b to primes less than 1000
-- here these primes are hard coded for speed up
list_b =[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 
         61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 
         137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 
         211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 
         293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 
         383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 
         463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 
         569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 
         647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 
         743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 
         839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 
         941, 947, 953, 967, 971, 977, 983, 991, 997]

isqrt :: (Integral b, Integral a) => a -> b
isqrt x = ceiling (sqrt(fromIntegral x))

-- Tests if a number is prime by division up to isqrt k
-- Special case included for k = 2
isPrime :: Integral a => a -> Bool
isPrime 2 = True
isPrime k = if k > 2 then null [ x | x <- [2..isqrt k], k `mod` x == 0] else False

-- foldl (&&) True [True,True,True,True,True,True,True,True,True,True,True] -> True
true_list :: Foldable t => t Bool -> Bool
true_list xs = foldl (&&) True xs

-- quad_list 1 41 10 -> [41,43,47,53,61,71,83,97,113,131,151]
quad_list :: (Num a, Enum a) => a -> a -> a -> [a]
quad_list a b lim = [n^2+a*n+b| n<-[0..lim]]

-- Given a pair a, b and a, test a b n 
-- returns true if i^2+a*i+b is prime 
-- for values 0<= i <= n
-- test 1 41 39 -> True
-- test 1 41 40 -> False
test :: Integral a => a -> a -> a -> Bool
test a b n = true_list$map isPrime$ quad_list a b n

-- val 1 41 -> 40
val :: (Integral a2, Num a1) => a2 -> a2 -> a1
val a b = sum[1|n<- [0..72],test a b n == True]

-- search for the max val in a restricted region 
-- 71 (1.39 secs, 694,001,544 bytes)
max_consec_prime :: Integer
max_consec_prime = maximum[val a b| a <- [(-70)..(-60)], b<-list_b, isPrime (1+a+b)]

--  answer -> [-59231]
answer :: [Integer]
answer = [(a*b)|a <- [(-70)..(-60)], b<-list_b, isPrime (1+a+b), val a b ==71]

-- main -59231
-- (1.33 secs, 689,683,792 bytes)
main :: IO ()
main = do  
    putStrLn$show$head answer

