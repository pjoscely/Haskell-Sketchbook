{-
'''
Square digit chains
 
Problem 92
A number chain is created by continuously adding the square of the digits 
in a number to form a new number until it has been seen before.

For example,

44 → 32 → 13 → 10 → 1 → 1
85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89

Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. 
What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.

How many starting numbers below ten million will arrive at 89?
'''
-- **************************************************************************************
-- Python Solution to "prime the pump":
#Hash set to store sums of squares leading to 89
arrive_89 = {89}

#Compute sum of squares of digits
def add_sqs(n):
    sum = 0
    for d in str(n):
        sum+=(int(d))**2
    return sum

#Tests if number is of type 89 
def does(n):
    orig = n
    if n in arrive_89:
        return True
    buffer = set()
    while(n != 1 and n != 89):
        n = add_sqs(n)
        if n in arrive_89:
            return True
        buffer.add(n)
    if(n == 89):
        arrive_89.add(orig)
        for item in buffer:
            arrive_89.add(item)
        return True
    if (n == 1):
        return False 

#The sum of the squares of the digits is bounded by 7*9**2
#Build a look up table
for i in range (1,7*9**2+1):
    if(does(i)):
         arrive_89.add(i)
#If            
ct= 0
for i in range(2, 10**7):
    if add_sqs(i) in arrive_89:
        ct+=1
print(ct)

'''
Congratulations, the answer you gave to problem 92 is correct.

You are the 40038th person to have solved this problem.
'''
-}
-- **************************************************************************************
-- Haskell Solution ~ 3 mins to complete
-- Could be made faster by creating a look up table 
-- for squares of digits 
import qualified Data.Set as Set

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

-- Compute sum of squares of a digits
-- sum_sqs 145 -> 42
sum_sqs :: Show a => a -> Int
sum_sqs n = sum$ map (\x -> x^2) digits_lst
          where digits_lst = list_of_digits n

-- Max value of 7 digit sum of squares
bound :: Int
bound =  7*9^2

-- Helper function to detect if n is type 89 or type 1
len_n :: Int -> Int
len_n n =  length$take 1$ filter (==89)$ take bound (iterate sum_sqs n)

-- Returns True if n is type 89 False otherwise
test :: Int -> Bool
test n = if len_n n == 1 then True else False

-- Form list of all type 89 numbers
arrive_89 :: [Int]
arrive_89 = [n| n<- [1..bound], test n] 
  
-- Convert to Set for O(log n) lookup
set_89 :: Set.Set Int
set_89 = Set.fromList arrive_89

-- Compute total number of type 89 less than 10 million
answer :: Integer
answer = sum[1| n<- [1..9999999], (Set.member (sum_sqs n) set_89)]

-- main-> 8581146
-- (174.04 secs, 318,349,133,352 bytes)
main :: IO ()
main = do  
    putStrLn$show answer











