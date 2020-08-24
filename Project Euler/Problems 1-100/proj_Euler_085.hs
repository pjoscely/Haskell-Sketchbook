{-
Counting rectangles
https://projecteuler.net/problem=85
Problem 85
By counting carefully it can be seen that a rectangular grid measuring 3 by 2 contains eighteen rectangles:


Although there exists no rectangular grid that contains exactly two million rectangles, 
find the area of the grid with the nearest solution.

-}
-- Helper fucntion to access the last element of a 4-tuple
fourth :: (a, b, c, d) -> d
fourth (a,b,c,d) = d

-- Combinatoric formula to count number of rectangles
-- num_rect 2 3 -> 18
num_rect :: Integral a => a -> a -> a
num_rect n m = (n*(n+1)*m*(m+1)) `div` 4

-- Minimizes the absolute distance between number of rectangles and 2 million
-- Displays a 4 tuple of difference, n, m, and area = n*m
-- Uses more than adequate search space 1..1000 for n, m
-- answer -> (2,36,77,2772)
answer :: (Integer, Integer, Integer, Integer)
answer = minimum [(abs ((num_rect n m) - 2*10^6),n, m, n*m)|n<-[1..1000],m<-[1..1000]]

-- main -> 2772
-- (5.44 secs, 1,997,145,696 bytes)
main :: IO ()
main = print $ fourth $ answer



-- Congratulations, the answer you gave to problem 85 is correct.

-- You are the 23683rd person to have solved this problem.


