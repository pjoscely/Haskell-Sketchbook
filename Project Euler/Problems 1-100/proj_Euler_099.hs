{-
'''
Largest exponential

Problem 99
Comparing two numbers written in index form like 2**11 and 3**7 
is not difficult, as any calculator would confirm that 211 = 2048 < 37 = 2187.

However, confirming that 632382**518061 > 519432**525806 would be much more difficult, 
as both numbers contain over three million digits.

Using base_exp.txt (right click and 'Save Link/Target As...'), 
a 22K text file containing one thousand lines with a base/exponent pair o
n each line, determine which line number has the greatest numerical value.

NOTE: The first two lines in the file represent the numbers in the example given above.
'''
***********************************************************************************
Python solution

import math
my_file = open("p099.txt", "r")
content = my_file.read()
lst = content.split("\n")
my_file.close()
max = 0 
ct = 0

for i in range(len(lst)):
    idx = lst[i].index(',')
    t = math.log(int(lst[i][0:idx]))*int(lst[i][idx+1:])
    if(t > max):
        max = t
        ct=i
print(ct+1)

'''
Congratulations, the answer you gave to problem 99 is correct.

You are the 29301st person to have solved this problem.
'''
-}
-- ************************************************************
-- Haskell solution
import System.IO  

-- Read base exponent pairs and stores each in a tuple
-- 525895,525320 -> (525895,525320)
readPair :: (Read a, Read b) => [Char] -> (a, b)
readPair s = let (a, b) = span (/= ',') s in (read a, read $ tail b)

-- Assign a index 1... to each log b*e values
-- Filter out max log b*e value
-- take second enrty of this tuple and print 
-- main -> 709
-- (0.05 secs, 14,241,856 bytes)
main :: IO ()
main = do
  values <- map readPair . lines <$> readFile "p099.txt"
  print $ snd $ maximum [(log b * e, i) | ((b, e), i) <- zip values [1..]]


