--Some of the functions below adapted from: Learn You a Haskell for Great Good!

--Recursive Maximum of a List

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs) = max x (maximum' xs)  

--Recursive Minimum of a List

minimum' :: (Ord a) => [a] -> a  
minimum' [] = error "minimum of empty list"  
minimum' [x] = x  
minimum' (x:xs) = min x (minimum' xs) 

--replicate 3 5 returns [5,5,5]

replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x 

--take' 3 [5,4,3,2,1] will return [5,4,3]

take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs 

--drop' 3 [5,4,3,2,1] will return [2,1]

drop' :: (Num i, Ord i) => i -> [a] -> [a] 
drop' _ []     = [] 
drop' n xs@(_:xs')
   | n > 0     = drop' (n-1) xs'
   | otherwise = xs

--Reverse a list
reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]  

--zip [1,2,3] [2,3] returns [(1,2),(2,3)]

zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys 

--elem' takes an element and a list and sees if that element is in the list.
--elem' 's' "snake" -> True

elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs   

--isPalin takes a String an determines if the String is a palindrome
--isPalin "radar" -> True
--isPalin "notapalindrome" -> False

isPalin :: String -> Bool
isPalin s 
    | s == reverse' s = True 
    | otherwise = False

--isPalin' takes a String an determines if the String is a palindrome
--isPalin' "radar" -> True
--isPalin' "notapalindrome" -> False
--isPalin' uses direct recursion, avoids the reverse function

isPalin' :: String -> Bool
isPalin' s
    | s == "" = True --base case
    | head s /= last s = False --if first and last characters are not equal return false
    | otherwise = isPalin'  (drop 1 (take (length s - 1) s)) --drop first and last tehn recurse


--Collatz number generator problem
--Given a positive integer returns a list 
--Containing repeated Collatz calls

--col computes one Collatz step
col :: Int -> Int
col n = if n `mod` 2 == 0
    then n `div` 2
    else 3*n+1

--fill list with complete collatz sequence
collatz :: Int -> [Int]
collatz 1 = [1]
collatz n = n:collatz (col n)

--Euclid’s algorithm
myGCD::Int -> Int -> Int
myGCD a b = if remainder == 0
            then b
            else myGCD b remainder
  where remainder = a `mod` b

--Quicksort algorithm
--quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9]  
--[1,2,2,3,3,4,4,5,6,7,8,9,10]  
--quicksort "the quick brown fox jumps over the lazy dog"  
--abcdeeefghhijklmnoooopqrrsttuuvwxyz"

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 


-- Recursively computes the number of ways needed
-- to solve a Tower of Hanoi puzzle with
-- n disks
-- f 64 -> 18446744073709551615
f :: (Eq t, Num t, Num p) => t -> p
f 0 = 0
f 1 = 1
f n = 2*f(n-1)+1

-- Gives the sequence of moves to solve a n disc 
-- Tower of Hanoi 
-- Here all discs are moved to peg "b"
-- hanoi 1  "a" "b" "c" -> [("a","b")]
-- hanoi 2  "a" "b" "c" -> [("a","c"),("a","b"),("c","b")]
-- hanoi 3 "a" "b" "c" -> 
-- [("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b")]
hanoi :: Integer -> a -> a -> a -> [(a, a)]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
