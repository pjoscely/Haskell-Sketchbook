{-

'''
Project Euler
https://projecteuler.net/

Passcode derivation

Problem 79
A common security method used for online banking is to ask the user 
for three random characters from a passcode. For example, 
if the passcode was 531278, they may ask for the 2nd, 3rd, and 5th characters; 
the expected reply would be: 317.

The text file, keylog.txt, contains fifty successful login attempts.

Given that the three characters are always asked for in order, 
analyse the file so as to determine the shortest possible secret passcode 
of unknown length.

***********************************************************************************
Python solution
'''
#50 3-digit strings
#lines = ['319', '680', '180', ...'319', '728', '716']
with open('p079.txt') as f:
    lines = [line.rstrip() for line in f]

#Remove duplicates
logs = set()
for item in lines:
    logs.add(item)


thisdictLT = {
  "0": [],
  "1": [],
  "2": [],
  "3": [],
  "4": [],
  "5": [],
  "6": [],
  "7": [],
  "8": [],
  "9": [],
}


thisdictGT = {
  "0": [],
  "1": [],
  "2": [],
  "3": [],
  "4": [],
  "5": [],
  "6": [],
  "7": [],
  "8": [],
  "9": [],
}

for item in logs:
    for d in "0123456789":
        if d in item:
            indx = item.index(d)
            LT = item[0:indx]
            GT = item[indx+1:]
            for l in LT:
                if l not in thisdictLT[d]:
                    thisdictLT[d].append(l)
            for g in GT:
                if g not in thisdictGT[d]:
                    thisdictGT[d].append(g)
            
for key, value in thisdictLT.items():
    print(key, ' : ', value)                 
print()
print()
for key, value in thisdictGT.items():
    print(key, ' : ', value)  

'''
This dictionary gives the lists of all digits that appear 
before the given digit in the login dat file
0  :  ['2', '9', '6', '8', '7', '1', '3']
1  :  ['3', '7']
2  :  ['6', '1', '7', '3']
3  :  ['7']
4  :  []
5  :  []
6  :  ['1', '7', '3']
7  :  []
8  :  ['6', '1', '3', '2', '7']
9  :  ['2', '8', '6', '7', '3', '1']

This dictionary gives the lists of all digits that appear 
after the given digit in the login data file
0  :  []
1  :  ['6', '8', '0', '2', '9']
2  :  ['9', '0', '8']
3  :  ['6', '8', '9', '1', '0', '2']
4  :  []
5  :  []
6  :  ['8', '0', '2', '9']
7  :  ['9', '0', '6', '3', '1', '8', '2']
8  :  ['0', '9']
9  :  ['0']   
    
From either dictionary the solution 73162890  
can be constructed
Congratulations, the answer you gave to problem 79 is correct.

You are the 39772nd person to have solved this problem. 

'''  
-}
-- ***********************************************************************************
-- Haskell Solution 
import Data.List
import qualified Data.Set as Set

log_file = ["290", "680", "620", "168", "890", "690", "790", "769", "368", "289", "389", "180", 
           "316", "318", "731", "760", "162", "716", "129", "710", "719", "718", "720", "629", "728", 
           "729", "319", "762", "736", "160", "380", "689", "362"]

-- ********************************************************
-- Various helper functions 
-- not all need to be used
-- int_to_string 637287 -> "637287"
int_to_string :: Show a => a -> String
int_to_string n = show n

-- convert_list "3456789" -> [3,4,5,6,7,8,9]
convert_list :: String -> [Int]
convert_list = map (read . return) . concat . lines

-- Convert integer to list of digits
-- list_of_digits 3456789 -> [3,4,5,6,7,8,9]
list_of_digits :: Show a => a -> [Int]
list_of_digits n = (convert_list . int_to_string) n

-- Convert String to Integer
-- convert_to_int "12345" -> 12345
convert_to_int :: String -> Integer
convert_to_int n = read n :: Integer

-- Convert integer digit list to Integer
-- convert_digit_list [1,2,3,4] -> 1234
convert_digit_list :: Show a => [a] -> Integer
convert_digit_list lst = convert_to_int$concat$ map show lst
-- ************************************************************
-- This returns all possible ways of inserting a new element into a list.
-- interleave 1 [2,3,4] -> [[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]]
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- This returns all permutations of a list, which are given by all possible
-- reorderings of the elements.
-- perms [1,2,3] -> [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- Tests if a triple of digits' order is preserved in the permuatation list 
-- test_perm [3,2,6,4,8,9] [2,4,9] -> True
test_perm :: Eq a => [a] -> [a] -> Bool
test_perm p s = (findIndex(==(s!!0)) p < findIndex(==(s!!1)) p) && (findIndex(==(s!!1)) p < findIndex(==(s!!2)) p)

-- Convert log_file to list of lists
-- log_file = ["290", "680", "620", "168", "890", ->
-- log_lst = [[2,9,0],[6,8,0],[6,2,0],[1,6,8],...
-- For a given permutation of "01236789" count
-- the number of matches in the log_file
filter_p :: Num a => [Int] -> a
filter_p p = sum[1|s<-map convert_list  log_file, test_perm p s]

-- Build list of all permuatations which match every tripe in the log_file
-- answer :: [[Int]]
answer = [p| p<-perms$convert_list "01236789", filter_p p == length log_file]

-- main -> [[7,3,1,6,2,8,9,0]]
-- (11.99 secs, 14,981,733,808 bytes)
main :: IO ()
main = do  
    (putStrLn.show) answer
