-- Project Euler
-- https://projecteuler.net/

-- '''
-- Counting Sundays
  
-- Problem 19
-- You are given the following information, but you may prefer to do some research for yourself.

-- 1 Jan 1900 was a Monday.
-- Thirty days has September,
-- April, June and November.
-- All the rest have thirty-one,
-- Saving February alone,
-- Which has twenty-eight, rain or shine.
-- And on leap years, twenty-nine.
-- A leap year occurs on any year evenly divisible by 4, but not on a century 
-- unless it is divisible by 400.
-- How many Sundays fell on the first of the month during the twentieth century 
-- (1 Jan 1901 to 31 Dec 2000)?
-- *****************************************************************************

-- Use Zeller's Rule

-- http://mathforum.org/dr.math/faq/faq.calendar.html

-- The following formula is named Zeller's Rule after a Reverend Zeller. 
-- [x] means the greatest integer that is smaller than or equal to x. 

-- f = k + [(13*m-1)/5] + D + [D/4] + [C/4] - 2*C.

-- k is the day of the month. Let's use January 29, 2064 as an example. 
-- For this date, k = 29.

-- m is the month number. Months have to be counted specially for Zeller's Rule: 
-- March is 1, April is 2, and so on to February, which is 12. 
-- (This makes the formula simpler, because on leap years February 29 is counted 
-- as the last day of the year.) Because of this rule, January and February are 
-- always counted as the 11th and 12th months of the previous year. 
-- In our example, m = 11.

-- D is the last two digits of the year. Because in our example we are using January 
-- D = 63 even though we are using a date from 2064.

-- C stands for century: it's the first two digits of the year. In our case, C = 20. 

-- A Python solution is given first
-- '''
-- #A remainder of 0 corresponds to Sunday, 1 means Monday,..., 6 means Saturday
-- def zeller(k,m,D,C):
--     f =  k + (13*m-1)//5 + D + D//4 + C//4 - 2*C
--     return f%7

--  #This counts Sundays falling on the first of the month for
--  #March to December from 1901 to 1999 inclusive 
--  ct_1 = 0
--  for m in range (1,11):
--     for D in range (1,100):
--        if zeller(1,m,D,19) == 0:
--             ct_1+=1
            
-- #This counts Sundays falling on the first of the month for
-- #january to February from 1901 to 1999 inclusive 
-- ct_2 = 0 
-- for m in range (11,13):
--     for D in range (1,100):
--         if zeller(1,m,D-1,19) == 0:
--             ct_2+=1

-- #This counts Sundays falling on the first of the month for
-- #March to December for the year 2000
-- ct_3 = 0
-- for m in range (1,11):
--     for D in range (0,1):
--         if zeller(1,m,D,20) == 0:
--             ct_3+=1

-- #Check January 1 2000 
-- print(zeller(1,11,99,19))
-- # = 6 a Saturday

-- #Check February 1 2000
-- print(zeller(1,12,99,19))
-- # = 2 a Tuesday

-- print(ct_1+ ct_2 +ct_3)

-- #171
-- # Congratulations, the answer you gave to problem 19 is correct.
-- # You are the 132171st person to have solved this problem.
-- *************************************************************************************
-- Haskell Solution

-- Zeller's Rule after a Reverend Zeller, describes above 
zeller :: Integral a => a -> a -> a -> a -> a
zeller k m d c = (k + (13*m-1)`div`5 + d + d`div`4 + c`div`4 - 2*c)`rem`7

--  This counts Sundays falling on the first of the month for
--  March to December from 1901 to 1999 inclusive 
ct_1 :: Integer
ct_1 = sum[1|m<-[1..10],d<-[1..99], zeller 1 m d 19 == 0]

-- This counts Sundays falling on the first of the month for
-- January to February from 1901 to 1999 inclusive 
ct_2 :: Integer
ct_2 = sum[1|m<-[11..12],d<-[1..99], zeller 1 m (d-1) 19 == 0]

-- This counts Sundays falling on the first of the month for
-- March to December for the year 2000
ct_3 :: Integer
ct_3 = sum[1|m<-[1..10],d<-[0], zeller 1 m d 20 == 0]

-- Compute total number of 1st of the month Sundays
answer :: Integer
answer = ct_1+ ct_2 +ct_3

-- main -> 171
-- (0.01 secs, 2,121,472 bytes)
main :: IO ()
main = do  
    putStrLn$show answer



