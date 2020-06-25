-- Project Euler
-- https://projecteuler.net/


-- Project Euler
-- https://projecteuler.net/

-- Lattice paths

-- Problem 15
-- Starting in the top left corner of a 2×2 grid, and only being able 
-- to move to the right and down, there are exactly 6 routes to the bottom 
-- right corner.


-- How many such routes are there through a 20×20 grid?
---answer: 137846528820

-- Congratulations, the answer you gave to problem 15 is correct.

-- You are the 183514th person to have solved this problem.

-- *********************************************************
-- Memoized Python solution

-- # memoized recursion 
-- items= dict()

-- def lat (x, y):
-- #   use tuple (x,y) as key in item dictionary
--     if (x,y) in items:
--       return items[(x,y)]
--    if x ==0 or y == 0:
--        return 1
--    else:
--        result = lat(x -1, y)+ lat(x , y -1)
--        items[(x,y)] = result
--       return result

-- print(lat(20,20))

-- *******************************************************************
-- The answer is the Catalan number 2n choose n
-- choose 40  20
-- 137846528820
-- (0.01 secs, 153,312 bytes)
fact :: (Eq p, Num p) => p -> p
fact 0 = 1
fact 1 = 1
fact n =n * fact (n-1)

-- Binomial coefficent 
choose :: Integral p => p -> p -> p
choose n k = (fact n) `div` ((fact k) * fact (n-k))

-- *******************************************************************
-- Too slow naive recursion solution
-- lattice_slow 10  10
-- 184756
-- (0.48 secs, 132,403,520 bytes)
lattice_slow 0 y = 1
lattice_slow x 0 = 1
lattice_slow x y = (lattice_slow (x - 1) y) + (lattice_slow x  (y -1))
-- *******************************************************************
-- Attempt at fast memoization -- eats memory and is slow!!!!!!
-- memoized_lattice 11 11
-- 705432
-- (1.96 secs, 1,174,760,864 bytes)
memoized_lattice :: (Eq t, Num t, Num a) => t -> Int -> a
memoized_lattice p = ((map (lattice p) [0..20])!!)             
    where lattice 0 y = 1
          lattice x 0 = 1
          lattice x y = memoized_lattice (x-1) y + memoized_lattice x (y-1) 








