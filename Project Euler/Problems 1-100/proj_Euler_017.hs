-- Project Euler
-- https://projecteuler.net/


-- Number letter counts
 
-- Problem 17
-- If the numbers 1 to 5 are written out in words: one, two, three, four, five, 
-- then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, 
-- how many letters would be used?


-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) 
-- contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. 
-- The use of "and" when writing out numbers is in compliance with British usage.

-- Create list of special numbers as strings
digits =["one","two","three","four","five","six","seven","eight","nine"]
special = ["ten","eleven","twelve", "thirteen", "fourteen", "fifteen", "sixteen","seventeen","eighteen","nineteen"]
two_digits = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

-- Create all other two digit numbers
-- ["twentyone","twentytwo","twentythree",...,ninetyseven","ninetyeight","ninetynine"]
all_other_two_digits = [x++y|x<-two_digits,y<-digits] 

-- Create list of:
-- ["onehundred","twohundred","threehundred","fourhundred","fivehundred","sixhundred",
-- "sevenhundred","eighthundred","ninehundred"] 
digit_hundred = [x++"hundred"|x<-digits]

-- Create list of:
-- ["onehundredandone","twohundredandtwo",...,"ninehundredandeight","ninehundredandnine"]
digit_hundred_digit= [x++"hundred"++"and"++y|x<-digits, y<-digits]

-- Create list:
-- ["onehundredandten","onehundredandeleven","onehundredandtwelve",...,,"ninehundredandseventeen","ninehundredandeighteen","ninehundredandnineteen"]
digit_hundred_special = [x++"hundred"++"and"++y|x<-digits,y<-special]

-- Create list:
-- ["onehundredandtwenty","onehundredandthirty","onehundredandforty",...,"ninehundredandseventy","ninehundredandeighty","ninehundredandninety"]
three_digits_two_digits = [x++"hundred"++"and"++y|x<-digits,y<-two_digits]

-- Create list:
-- ["onehundredandtwentyone","onehundredandtwentytwo",...,"ninehundredandninetyeight","ninehundredandninetynine"]
all_other_three_digits = [x++"hundred"++"and"++y|x<-digits,y<-all_other_two_digits]

-- Lastly "onethousand"
one_thousand = ["onethousand"]

-- Finds sums of length of the ten different lists

-- 36
s1 = sum$ map length digits

-- 70
s2 = sum$ map length special

-- 46
s3 = sum$ map length two_digits

-- 702
s4 = sum$ map length all_other_two_digits

-- 99
s5 = sum$ map length digit_hundred

-- 1458
s6 = sum$ map length digit_hundred_digit

-- 1890
s7 = sum$ map length digit_hundred_special

--1422
s8 = sum$ map length three_digits_two_digits

-- 15390
s9 = sum$ map length all_other_three_digits

-- 11
s10 = sum$ map length one_thousand

answer = s1+s2+s3+s4+s5+s6+s7+s8+s9+s10

-- main -> 21124
-- (0.00 secs, 117,560 bytes)
main :: IO ()
main = do  
    putStrLn$ show answer 

-- Congratulations, the answer you gave to problem 17 is correct.

-- You are the 148476th person to have solved this problem.



