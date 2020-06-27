-- Project Euler
-- https://projecteuler.net/

-- Longest Collatz sequence

-- Problem 14
-- The following iterative sequence is defined for the set of positive integers:

-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)

-- Using the rule above and starting with 13, we generate the following sequence:

-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
-- It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

-- Which starting number, under one million, produces the longest chain?

-- NOTE: Once the chain starts the terms are allowed to go above one million.

-- recursive Collatz taken from http://learnyouahaskell.com/higher-order-functions

import Data.List
import Data.Maybe

chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)  


-- compute list of chains 
-- [1,2,8,3,.......,
list_of_chains :: [Int]
list_of_chains = map length$map chain [1..999999]

-- Compute the maximum length chain
-- length = 525
max_chain :: Int
max_chain = maximum$list_of_chains 

-- Find the index of 525, the solution is one more than the index
solution :: Int
solution = (fromJust $ elemIndex max_chain list_of_chains) + 1 


-- Display the answer
-- 837799
-- (288.08 secs, 84,641,580,832 bytes)
main :: IO ()
main = do  
    putStrLn$show solution

-- Congratulations, the answer you gave to problem 14 is correct.

-- You are the 223055th person to have solved this problem.

