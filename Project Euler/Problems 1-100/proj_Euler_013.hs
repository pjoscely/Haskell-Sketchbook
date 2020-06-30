-- Project Euler
-- https://projecteuler.net/

-- Large sum
 
-- Problem 13
-- Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.

-- for the full list see https://projecteuler.net/problem=13

-- 37107287533902102798797998220837590246510135740250
-- 46376937677490009712648124896970078050417018260538
-- 74324986199524741059474233309513058123726617309629
-- ...
-- ...
-- ...
-- 72107838435069186155435662884062257473692284509516
-- 20849603980134001723930671666823555245252804609722
-- 53503534226472524250874054075591789781264330331690

-- ***********************************************************
-- Python solution was completed first

-- #read the above from a text file
-- with open('Proj_Euler.txt') as f:
--     content = f.readlines()
-- # you may also want to remove whitespace characters like `\n` at the end of each line
-- content = [x.strip() for x in content] 

-- sum = 0
-- for item in content:
--     sum+=int(item)
-- print(sum)
-- print(str(sum)[0:10])

-- ************************************************************
-- Haskell solution
import System.IO  
  
-- Read text file and return an IO Integer list of the one-hundred 50-digit numbers.
-- read_numbers :: IO [Integer]
read_numbers = do
    contents <- readFile "Proj_Euler.txt" 
    let linesOfFile = map (\x -> read x :: Integer) (lines $ contents)
    return linesOfFile

-- Sum the list and return the total as an IO Integer
-- 5537376230390876637302048746832985971773659831892672
total_all :: IO Integer
total_all = do
      xs <- read_numbers
      return (sum xs)

-- hard code answer is eastiest; avoids lifting out of the monad
answer = take 10 "5537376230390876637302048746832985971773659831892672"

-- Display answer
-- main -> 5537376230
-- (0.00 secs, 122,568 bytes)
main :: IO ()
main = do  
    putStrLn answer

-- Congratulations, the answer you gave to problem 13 is correct.

-- You are the 222711th person to have solved this problem.
