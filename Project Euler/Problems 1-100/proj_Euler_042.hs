{-
Coded triangle numbers
https://projecteuler.net/problem=42
Problem 42
The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its alphabetical position and adding these values
 we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. 
 If the word value is a triangle number then we shall call the word a triangle word.

Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand 
common English words, how many are triangle words?

words.txt = ["A","ABILITY","ABLE","ABOUT","ABOVE","ABSENCE","ABSOLUTELY","ACADEMIC",
       "ACCEPT","ACCESS","ACCIDENT","ACCOMPANY","ACCORDING","ACCOUNT","ACHIEVE",
       "ACHIEVEMENT","ACID","ACQUIRE","ACROSS","ACT","ACTION","ACTIVE","ACTIVITY",
       "ACTUAL","ACTUALLY","ADD","ADDITION","ADDITIONAL","ADDRESS","ADMINISTRATION",
       "ADMIT","ADOPT","ADULT","ADVANCE","ADVANTAGE","ADVICE","ADVISE","AFFAIR","AFFECT",...,
       ..., ,"WRITE","WRITER","WRITING","WRONG","YARD","YEAH","YEAR","YES","YESTERDAY",
       "YET","YOU","YOUNG","YOUR","YOURSELF","YOUTH"]

-}


import Data.Char
import Data.List

-- Possible list of traingle values; predicated on bounded word size
tri_lst :: [Integer]
tri_lst = [0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105, 120,136, 153, 171, 190, 
           210, 231, 253, 276, 300, 325, 351, 378, 406, 435, 465, 496, 528, 561, 595, 630, 
           666, 703, 741, 780, 820, 861, 903, 946, 990, 1035, 1081, 1128, 1176, 1225, 1275, 
           1326, 1378, 1431]

-- Tests if a number is triangular 
-- istriangle 276 -> True
istriangle :: Integer -> Bool
istriangle n = if n `elem` tri_lst then True else False
  
-- Computes the sum of alphabetic positions of letters in a string
-- sum_lettrs "SKY" -> 55
sum_lettrs :: [Char] -> Int    
sum_lettrs  s = sum $ map (\c -> (ord c) - 64) s

-- Helper method to remove control characters from the input string
-- parse "\"YEAR\",\"YES\",\"YESTERDAY\",\"YET\",\"YOU\",\"YOUNG\",\"YOUR\",\"YOURSELF\",\"YOUTH\"" ->
-- ["YEAR","YES","YESTERDAY","YET","YOU","YOUNG","YOUR","YOURSELF","YOUTH"]
parse :: String -> [String]
parse = words . map replaceComma . filter notQuote where
    replaceComma ',' = ' '
    replaceComma c = c
    notQuote = (/= '"')


-- 162 -> (0.04 secs, 4,593,928 bytes)
main :: IO ()  
main = do  
       str <- readFile "p042_words.txt"
       print$length$filter istriangle$ map fromIntegral $map sum_lettrs $parse str



