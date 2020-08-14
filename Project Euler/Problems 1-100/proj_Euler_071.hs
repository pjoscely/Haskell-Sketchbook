{-
Ordered fractions

Problem 71
Consider the fraction, n/d, where n and d are positive integers. 
If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:

1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that 2/5 is the fraction immediately to the left of 3/7.

By listing the set of reduced proper fractions for d ≤ 1,000,000 in ascending order of size, 
find the numerator of the fraction immediately to the left of 3/7.
-}
import Data.List
import qualified Data.Set as Set
-- Euclidean algorithm function
-- gc 124 56 -> 4
gc :: Integer -> Integer -> Integer
gc a b
      | b == 0     = abs a
      | otherwise  = gc b (a `mod` b)

-- Returns a tuple representing a reduced fraction
-- reduce_frac 124  144 -> (31,36)
reduce_frac :: Integer -> Integer -> (Integer, Integer)
reduce_frac a b = (a `div` d, b `div` d)
                  where d = gc a b

-- Compares fractions in terms of their tuple representation
-- An exercise here, not used, but included for reference
-- 2/7 < 5/6 iff 2*6 < 5*7
-- comp (2,7) (5,6) -> "<"
-- comp :: (a1, b1) -> (a2, b2) -> [Char]
comp :: (Ord a, Num a) => (a, a) -> (a, a) -> [Char]
comp (a,b) (c,d)
    | a' * d' < b' * c' = "<"
    | a' * d' > b' * c' = ">"
    | otherwise = "="
    where a' = fst (a,b)
          b' = snd (a,b)
          c' = fst (c,d)
          d' = snd (c,d)

-- Create list of reduced proper fractions in tuple form.
-- Interesting exercise, but too slow for the problem
-- lst_red 8 ->
-- fromList [(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(2,3),(2,5),(2,7),(3,4),
-- (3,5),(3,7),(3,8),(4,5),(4,7),(5,6),(5,7),(5,8),(6,7),(7,8)]
lst_red d = Set.fromList [reduce_frac n d  | n <- [1..(d-1)], d<-[n+1..d]]


-- Uses theory of Farey (Haros) Sequences 
-- https://en.wikipedia.org/wiki/Farey_sequence
-- Compute best mediant using tuple representation
-- med (2,5) (3,7) -> (5,12)
-- 2/5 < 5/12 < 3/7
-- 5/12 < 8/19 < 3/7
-- 8/19 < 11/26 < 3/7
-- ...
-- answer -> 428570
answer = maximum [a |k<-[1..10^6],let  a = 2+3*k,let b = 5+7*k, b<=10^6]  
-- main -> 428570
-- (1.89 secs, 1,022,160,736 bytes)
main :: IO ()
main = do  
    (putStrLn.show) answer


