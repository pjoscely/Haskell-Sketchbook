-- Project Euler
-- https://projecteuler.net/

-- Integer right triangles

-- Problem 39
-- If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, 
-- there are exactly three solutions for p = 120.

-- {20,48,52}, {24,45,51}, {30,40,50}

-- For which value of p ≤ 1000, is the number of solutions maximised?


import Data.List
import Data.Maybe
-- return filtered list, parametrized by p perimeter
-- [(20,48,52),(24,45,51),(30,40,50)]
tri_list :: (Ord c, Enum c, Num c) => c -> [(c, c, c)]
tri_list p =  [(a,b,c)|a<-[3..p],b<-[4..p],let c = p - a - b, a < b + c, b < a + c, c < a + b, a < b, b < c, a^2+b^2==c^2]

-- Compute list of list of solutions
list_of_lists :: [[(Integer, Integer, Integer)]]
list_of_lists = map tri_list [12..1000]


-- Filter out empty lists 
-- [[(3,4,5)],[(6,8,10)],[(5,12,13)],[(9,12,15)],...,[(31,480,481)],[(249,332,415)],[(200,375,425)]]
-- The aabove is a very large list of lists, so only the start and end are shown
filter_list_of_lists :: [[(Integer, Integer, Integer)]]
filter_list_of_lists= filter (/=[]) list_of_lists

-- find the maximum length list in filter_list_of_lists-> 8
max_length :: Int
max_length = maximum$map length filter_list_of_lists

-- create list of lengths 
-- [1,1,1,1,1,1,1,2,1,1,1,2,2,1,1,1,3,1,2,1,2,1,1,1,1,3,1,3,1,1,1,1,
-- 1,1,2,1,1,1,1,1,4,3,1,2,2,1,3,1,2,2,1,1,2,1,1,2,3,1,1,1,1,4,1,1,1,1,
-- 1,1,2,1,3,1,2,1,1,5,2,2,1,1,1,2,2,2,2,1,4,1,1,1,4,2,1,2,3,1,3,1,2,2,
-- 3,1,2,1,2,2,1,1,3,1,2,2,3,4,1,1,1,1,1,1,5,4,2,2,2,1,2,1,1,1,1,6,2,1,
-- 1,1,1,1,4,2,1,2,4,1,1,3,1,2,1,2,2,2,1,1,8,1,1,1,3,2,1,1,3,1,1,1,1,4,
-- 2,2,2,2,5,1,1,3,1,1,2,4,1,1,1,1,1,1,4,1,1,1]
list_lengths :: [Int]
list_lengths = map length filter_list_of_lists

-- index_of_max = 154
-- Finding an index is not proper Haskell,
-- nonetheless it solves the problem
index_of_max :: Int
index_of_max = fromJust $ elemIndex 8 list_lengths

-- [(40,399,401),(56,390,394),(105,360,375),(120,350,370),(140,336,364),
-- (168,315,357),(210,280,350),(240,252,348)]
list_of_triples :: [(Integer, Integer, Integer)]
list_of_triples =filter_list_of_lists !! index_of_max

-- Create sum of triples function
total :: Num a => (a, a, a) -> a
total (a,b,c) = a+b+c

-- map total on list_of_triples
-- [840,840,840,840,840,840,840,840]
-- (236.88 secs, 101,663,869,968 bytes)
-- so the common perimeter = 840 for 8 (mx number) different triples 
perimeter_list :: [Integer]
perimeter_list = map total list_of_triples 

-- Congratulations, the answer you gave to problem 39 is correct.

-- You are the 70977th person to have solved this problem.


