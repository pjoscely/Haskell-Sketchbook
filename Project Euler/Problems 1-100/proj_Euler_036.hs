{-

'''
 Project Euler
 https://projecteuler.net/
Double-base palindromes
 
Problem 36
The decimal number, 585 = 1001001001(binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic 
in base 10 and base 2.

(Please note that the palindromic number, in either base, 
 may not include leading zeros.)
**************************************************************************************
Python Solution to "prime the pump":
'''
# Function to reverse a string 
def rev(string): 
    string = string[::-1] 
    return string 

#Display and count Double-base palindromes
ct = 0
for i in range(10**6):
    str_10 = str(i)
    str_2 = "{0:b}".format(i)
    if(str_10[-1] != '0' and  str_2[-1] != '0' and  str_10 == rev(str_10) and str_2 == rev(str_2)):
        ct+=i
        print(str_10,str_2)
print(ct)
'''
0 0
1 1
3 11
5 101
7 111
9 1001
33 100001
99 1100011
313 100111001
585 1001001001
717 1011001101
7447 1110100010111
9009 10001100110001
15351 11101111110111
32223 111110111011111
39993 1001110000111001
53235 1100111111110011
53835 1101001001001011
73737 10010000000001001
585585 10001110111101110001
872187
Congratulations, the answer you gave to problem 36 is correct.

You are the 86806th person to have solved this problem.
'''
-}
-- ************************************************************
-- Haskell solution

-- Convert decimal to binary
-- toBin 234 -> [1,1,1,0,1,0,1,0]
toBin :: (Integral a1, Num a2) => a1 -> [a2]
toBin 0 = [0]
toBin n = reverse (helper n)

-- helper 234 -> [0,1,0,1,0,1,1,1]
helper :: (Integral a1, Num a2) => a1 -> [a2]
helper 0 = []
helper n | n `mod` 2 == 1 = 1 : helper (n `div` 2)
         | n `mod` 2 == 0 = 0 : helper (n `div` 2)

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

-- Test if n is a proper decimal palindrome
-- is_dec 585585 -> True
is_dec :: Show a => a -> Bool
is_dec n = if (head digits_lst) /= 0 && digits_lst == reverse digits_lst  then True else False
          where digits_lst = list_of_digits n

-- Test if n is a proper binary palindrome
-- is_bin 99 -> True  since 99 = 1100011
is_bin :: Integral a1 => a1 -> Bool
is_bin n = if (head bin_digits_lst) /= 0 && bin_digits_lst == reverse bin_digits_lst  then True else False
          where bin_digits_lst =  toBin n

-- Filter and sum all solutions
solution :: Integer
solution = sum[n| n<-[0..999999], is_dec n,  is_bin n]

main :: IO ()
main = do  
    putStrLn$show solution
-- main = 872187










