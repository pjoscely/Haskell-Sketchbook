{-
-- Project Euler
-- https://projecteuler.net/
'''
Distinct powers
 
Problem 29

Consider all integer combinations of ab for 2 ≤ a ≤ 5 and 2 ≤ b ≤ 5:

2**2=4, 2**3=8, 2**4=16, 2**5=32
3**2=9, 3**3=27, 3**4=81, 3**5=243
4**2=16, 4**3=64, 4**4=256, 4**5=1024
5**2=25, 5**3=125, 5**4=625, 5**5=3125

If they are then placed in numerical order, with any repeats removed, 
we get the following sequence of 15 distinct terms:

4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125

How many distinct terms are in the sequence generated by a**b for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?
'''
********************************************************************************************
Python solution first
dist_terms = set()

for a in range(2,101):
    for b in range(2,101):
        dist_terms.add(a**b)
        
        
print(len(dist_terms))

# Congratulations, the answer you gave to problem 29 is correct.

# You are the 103079th person to have solved this problem.
-}
-- ********************************************************************************************
-- Haskell solution 
import Data.Set 
-- Create unfiltered list of powers
list_of_powers :: [Integer]
list_of_powers = [a^b|a<-[2..100],b<-[2..100]]

-- Create set to remove duplicates 
set_of_powers :: Set Integer
set_of_powers = fromList list_of_powers 

-- Compute number of disticnt terms
num_distinct :: Int
num_distinct = length set_of_powers

-- (0.03 secs, 22,333,048 bytes)
-- main -> 9183
main :: IO ()
main = do  
    putStrLn$show num_distinct












