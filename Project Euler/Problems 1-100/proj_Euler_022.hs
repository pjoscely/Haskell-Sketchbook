-- Project Euler
-- https://projecteuler.net/

-- Names scores
  
-- Problem 22
-- Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing 
-- over five-thousand first names, begin by sorting it into alphabetical order. 
-- Then working out the alphabetical value for each name, multiply this value by its 
-- alphabetical position in the list to obtain a name score.

-- For example, when the list is sorted into alphabetical order, COLIN, 
-- which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. 
-- So, COLIN would obtain a score of 938 × 53 = 49714.

-- What is the total of all the name scores in the file?
-- ***************************************************************************************
-- Python solution first

-- # Read 46K text file containing over five-thousand first names
-- my_file = open('p022_names.txt', "r")
-- content = my_file.read()
-- content_list = content.split(",")
-- my_file.close()

-- content_list.sort()

-- print(content_list)

-- #print(content_list.index('COLIN'))

-- def char_position(letter):
--    return ord(letter) - 64

-- def word_value(word):
--     sum = 0
--     for letter in word:
--         sum+=char_position(letter)
--     return sum

-- print(content_list.index('COLIN')+1)
-- print(word_value('COLIN'))


-- total = 0
-- for name in content_list:
--     indx = content_list.index(name)+1
--     value = word_value(name)
 --    total+= indx*value
    
-- print(total)

-- #Congratulations, the answer you gave to problem 22 is correct.

-- #You are the 131626th person to have solved this problem.

-- ***************************************************************************************
-- Haskell solution
import Data.Char (ord)
import Data.List (sort)

-- main -> 871198282 (0.17 secs, 92,826,376 bytes)
main            ::  IO ()
main            =   do  names <- readFile "p022_raw_names.txt"
                        let namelist = makeList names
                        print $ totalScores namelist
-- Build sorted list of tuples (position, name)
makeList        ::  Integral a => String -> [(a, String)]
makeList n      =   zip [1..] (sort . read $ n')
    where   n'          =   "[" ++ n ++ "]"

-- sum scores of each tuple (938,"COLIN") -> 938 × 53 = 49714
totalScores     ::  Integral a => [(a, String)] -> a
totalScores l   =   (sum . map score) l
    where   score (a,b) =   a * (score' b)
            score'      =   sum . map (\c -> (ord' c) - 64)
            ord'        =   fromIntegral . ord


