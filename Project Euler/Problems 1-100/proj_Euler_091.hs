{-
Project Euler
https://projecteuler.net/


Right triangles with integer coordinates
Problem 91
The points P (x1, y1) and Q (x2, y2) are plotted at integer co-ordinates 
and are joined to the origin, O(0,0), to form ΔOPQ.


There are exactly fourteen triangles containing a right angle that can be 
formed when each co-ordinate lies between 0 and 2 inclusive; that is,
0 ≤ x1, y1, x2, y2 ≤ 2.


Given that 0 ≤ x1, y1, x2, y2 ≤ 50, how many right triangles can be formed?
-}
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map

-- Distance formula
dist :: Floating a => a -> a -> a -> a -> a
dist x1 y1 x2 y2 = sqrt $ (x2 -x1)^2 + (y2 -y1)^2

-- Distance formula: point to origin
f :: Floating a => a -> a -> a
f a b = dist 0 0 a b

-- Tests if a pair of points and (0,0) forms a triangle 
is_tri :: (Ord a, Floating a) => a -> a -> a -> a -> Bool
is_tri x1 y1 x2 y2 = f x1 y1 + f x2 y2 > dist x1 y1 x2 y2 &&
                     f x1 y1 + dist x1 y1 x2 y2 > f x2 y2 &&
                     f x2 y2 + dist x1 y1 x2 y2 > f x1 y1 

-- Test if a pair of points and (0,0) forms a right triangle
is_right :: (Floating a, Ord a) => a -> a -> a -> a -> Bool
is_right x1 y1 x2 y2 = (abs ((f x1 y1)^2 + (f x2 y2)^2 - (dist x1 y1 x2 y2)^2) < 0.00000001) ||
                       (abs ((f x1 y1)^2 + (dist x1 y1 x2 y2)^2 - (f x2 y2)^2) < 0.00000001) ||
                       (abs ((f x2 y2)^2 + (dist x1 y1 x2 y2)^2 - (f x1 y1)^2) < 0.00000001)

-- 7 minutes! 
-- answer -> 14234
-- (429.07 secs, 260,774,208,456 bytes)
-- Use 0.5 to avoid counting twice
-- answer :: Double
answer = sum[0.5| x1<-[0..50],x2<-[0..50],y1<-[0..50],y2<-[0..50],(x1, y1) /= (x2, y2), is_tri x1 y1 x2 y2, is_right x1 y1 x2 y2]

--Congratulations, the answer you gave to problem 91 is correct.

--You are the 14602nd person to have solved this problem.

