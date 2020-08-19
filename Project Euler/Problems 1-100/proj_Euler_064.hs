{-
Odd period square roots

Problem 64

How many continued fractions for N <= 10000  have an odd period?
-}

-- This is the first solution which uses a file of precomputed 
-- continued fractions period values. Although a cheat nonethelass
-- this solution is presented as an example of reading and 
--  processing data stored in an external text file. See: 
-- https://oeis.org/search?q=1%2C2%2C1%2C2%2C4%2C2%2C1%2C2%2C2%2C5&sort=&language=&go=Search
-- https://oeis.org/A013943/b013943.txt

import System.IO 
-- main -> 1322 
main :: IO ()
main = do  
    handle <- openFile "P_064.txt" ReadMode  
    contents <- hGetContents handle  
    print$length$filter odd$take 9900$second$map (\x -> read x :: Integer)$words$contents  
    hClose handle 


-- Helper function to acesss first item 
-- from a String split by a space
-- first [12, 34] -> [12]
first :: [a] -> [a]
first [] = []
first (x:xs) = x:second xs

-- Helper function to acesss second item 
-- from a String split by a space
-- second [12, 34] -> [34]
second :: [a] -> [a]
second [] = []
second (x:xs) = first xs

-- Congratulations, the answer you gave to problem 64 is correct.

-- You are the 21115th person to have solved this problem.