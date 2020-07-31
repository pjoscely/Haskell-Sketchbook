{-

Project Euler
https://projecteuler.net/

Large non-Mersenne prime

Problem 97
The first known prime found to exceed one million digits was discovered in 1999, 
and is a Mersenne prime of the form 26972593−1; it contains exactly 2,098,960 digits. 
Subsequently other Mersenne primes, of the form 2p−1, have been found which contain more digits.

However, in 2004 there was found a massive non-Mersenne prime 
which contains 2,357,207 digits: 28433×2^7830457+1.

Find the last ten digits of this prime number.

-}
-- Straight forward computation in Haskell
answer :: Integer
answer = (28433*(2^7830457)+1)`mod`(10^10)

-- main -> 8739992577
-- (0.07 secs, 5,850,024 bytes)
main :: IO ()
main = do  
    (putStrLn.show) answer

-- Congratulations, the answer you gave to problem 97 is correct.

-- You are the 41913th person to have solved this problem.