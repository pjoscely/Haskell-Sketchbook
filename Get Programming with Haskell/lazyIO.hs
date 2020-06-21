-- Lesson 22. Interacting with the command line and lazy I/O


-- *Access command-line arguments
-- *Use the traditional approach to interacting through I/O
-- *Write I/O code using lazy evaluation to make I/O easier

import System.Environment
import Control.Monad

main1 :: IO ()
main1 = do
 args <- getArgs
 mapM_ putStrLn args

-- Q1:

-- Write a main that uses mapM to call getLine three times, 
-- and then use mapM_ to print out the values’ input. 
-- (Hint: You’ll need to throw away an argument when using mapM with 
-- getLine; use (\_ -> ...) to achieve this.)
exampleMain :: IO ()
exampleMain = do
   vals <- mapM (\_ -> getLine) [1..3]
   mapM_ putStrLn vals


-- Using a command-line argument to determine how many lines to read
main2 :: IO ()
main2 = do
 args <- getArgs
 let linesToRead = if length args > 0
                   then read (head args)
                   else 0 :: Int
 print linesToRead


-- Reading a number of lines equal to the user’s argument
main3 :: IO ()
main3 = do
 args <- getArgs
 let linesToRead = if length args > 0
                   then read (head args)
                   else 0
 numbers <- replicateM linesToRead getLine
 print "sum goes here"


main4 :: IO ()
main4 = do
 args <- getArgs
 let linesToRead = if length args > 0
                   then read (head args)
                   else 0 :: Int
 numbers <- replicateM linesToRead getLine
 let ints = map read numbers :: [Int]
 print (sum ints)


-- Q1:

-- Write your own version of replicateM, myReplicateM, 
-- that uses mapM. (Don’t worry too much about the type signature.)
myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM n func = mapM (\_ -> func) [1 .. n]


--  A simple main to explore lazy I/O
main5 :: IO ()
main5 = do
  userInput <- getContents
  mapM_ print userInput


-- Q1:

-- Use lazy I/O to write a program that reverses your input and prints it back to you.

reverser :: IO ()
reverser = do
   input <- getContents
   let reversed = reverse input
   putStrLn reversed


-- toInts function to convert your Char list into a list of Ints
toInts :: String -> [Int]
toInts = map read . lines


-- Q1:

-- Write a program that returns the sum of the squares of the input.

mainSumSquares :: IO ()
mainSumSquares = do
   userInput <- getContents
   let numbers = toInts userInput
   let squares = map (^2) numbers
   print (sum squares)

-- Q22.2

-- Write a program that allows a user to select a number between 1 and 5 and 
-- then prints a famous quote (quotes are of your choosing).

quotes :: [String]
quotes = ["All limitations are self-imposed."
         ,"Die with memories, not dreams."
         ,"There is no substitute for hard work."
         ,"Oh, the things you can find, if you don’t stay behind."
         ,"Nothing succeeds like excess."]

lookupQuote :: [String] -> [String]
lookupQuote [] = []
lookupQuote ("n":xs) = []
lookupQuote (x:xs) = quote : (lookupQuote xs)
  where quote = quotes !! (read x - 1)

main6 :: IO ()
main6 = do
  userInput <- getContents
  mapM_ putStrLn (lookupQuote  (lines userInput))
