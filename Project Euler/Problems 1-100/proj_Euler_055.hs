{-

Project Euler
https://projecteuler.net/

Lychrel numbers

Problem 55
If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.

Not all numbers produce palindromes so quickly. For example,

349 + 943 = 1292,
1292 + 2921 = 4213
4213 + 3124 = 7337

That is, 349 took three iterations to arrive at a palindrome.

Although no one has proved it yet, it is thought that some numbers, like 196, 
never produce a palindrome. A number that never forms a palindrome through 
the reverse and add process is called a Lychrel number. 
Due to the theoretical nature of these numbers, 
and for the purpose of this problem, we shall assume that a number is 
Lychrel until proven otherwise. 
In addition you are given that for every number below ten-thousand,  it will either 
(i) become a palindrome in less than fifty iterations, or, 
(ii) no one, with all the computing power that exists, 
has managed so far to map it to a palindrome. 
In fact, 10677 is the first number to be shown to require over fifty iterations before producing a palindrome: 
4668731596684224866951378664 (53 iterations, 28-digits).

Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; 
the first example is 4994.

How many Lychrel numbers are there below ten-thousand?
-}

-- Reverses an Integer
-- revNum 2345 -> 5432
revNum :: Integer -> Integer
revNum = read . reverse . show

-- Tests if a positive integer is a plaindrome
-- is_palin 121 -> True
is_palin :: Integer -> Bool
is_palin n = n == revNum n
-- Adds a positive integer and its reverse
-- 47 + 74 = 121
-- combine 47 -> 121
combine :: Integer -> Integer
combine n = n + revNum n

-- Tests if a number is Lychrel using the less than 50 iteration rule
-- is_Ly 196 -> True
-- is_Ly 4994 -> False
-- Use drop 1 to not count palindromes which are Lychrel
is_Ly :: Integer -> Bool
is_Ly n = if (filter (== True)$map is_palin$drop 1$take 50 (iterate (combine) n)) == [] then True else False

-- Compute answer 
answer :: Int
answer = length [n| n<-[196..9999], is_Ly n]

-- main ->249
-- (0.36 secs, 557,770,368 bytes)
main :: IO ()
main = do  
    (putStrLn.show) answer

-- Congratulations, the answer you gave to problem 55 is correct.

-- You are the 52169th person to have solved this problem.
