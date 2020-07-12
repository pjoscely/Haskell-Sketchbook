{-
'''
Powerful digit sum
 
Problem 56
A googol (10100) is a massive number: one followed by one-hundred zeros; 
100100 is almost unimaginably large: one followed by two-hundred zeros. 
Despite their size, the sum of the digits in each number is only 1.

Considering natural numbers of the form, a**b, where a, b < 100, 
what is the maximum digital sum?
-- ***************************************************************************
-- Python Solution to "prime the pump":
'''
#Return digit sum
def digit_sum(num):
    d_sum = 0
    s = str(num)
    for ch in s:
        d_sum+=int(ch)
    return d_sum

#use max for lower bound
max = -1
for a in range(1,100):
    for b in range(1,100):
        if digit_sum(a**b) > max:
            max = digit_sum(a**b)
print(max)
'''
Congratulations, the answer you gave to problem 56 is correct.

You are the 56410th person to have solved this problem.
''' 
-} 
-- ***************************************************************************
-- Haskell Solution 
-- Form all possible powers
list_of_powers :: [Integer]
list_of_powers = [a^b|a<-[1..99],b<-[1..99]]

-- convert_list "3456789" -> [3,4,5,6,7,8,9]
convert_list :: String -> [Int]
convert_list = map (read . return) . concat . lines

-- Create string version of num_lst
-- [123,456] -> ["123", "456"]
num_lst_string :: [String]
num_lst_string = map show list_of_powers

-- Expand each string into a digit list 
-- ["123", "456"] -> [[1,2,3],[4,5,6]]
list_of_digit_list :: [[Int]]
list_of_digit_list = map convert_list num_lst_string 

-- Form list of sums
-- [[1,2,3],[4,5,6]] ->[6,15]
list_of_digit_sums :: [Int]
list_of_digit_sums = map sum list_of_digit_list 

-- find maximum digit sum
answer :: Int
answer = maximum list_of_digit_sums 

-- (1.33 secs, 3,121,742,104 bytes)
main :: IO ()
main = do  
    putStrLn$show answer


