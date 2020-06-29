-- Project Euler
-- https://projecteuler.net/

-- Lexicographic permutations
 
-- Problem 24
-- A permutation is an ordered arrangement of objects. For example, 
-- 3124 is one possible permutation of the digits 1, 2, 3 and 4. 
-- If all of the permutations are listed -- numerically or alphabetically, 
-- we call it lexicographic order. 
-- The lexicographic permutations of 0, 1 and 2 are:

-- 012   021   102   120   201   210

-- What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?


-- Congratulations, the answer you gave to problem 24 is correct.

-- You are the 112047th person to have solved this problem.

import Data.List

-- recursive permutation generator
permute :: String -> [String]
permute [] = [[]]
permute str = do
    x  <- str
    xs <- permute (delete x str)
    return (x:xs)

-- sort the list permuations and extract the millionth item
soln :: String
soln = (sort$permute "0123456789")!!999999

-- main -> 2783915460
-- (25.05 secs, 11,828,545,464 bytes)
main :: IO ()
main = do  
    putStrLn soln

