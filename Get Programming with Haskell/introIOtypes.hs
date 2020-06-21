-- Lesson 21. Hello World!—introducing IO types


-- *Understand how Haskell handles I/O by using IO types
-- *Use do-notation to perform I/O
-- *Write pure programs that interact with the real world


import qualified Data.Map as Map
import Data.Maybe
import System.Random

minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6
-- main is not a function, rather it is an IO action
-- main --> 1..6
main :: IO ()
main = do
  dieRoll <- randomRIO (minDie,maxDie)
  putStrLn (show dieRoll)


--  An example: command-line pizza cost calculator

-- Calculating the area of a pizza given its diameter
areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi*(size/2)^2

-- Pizza type synonym size/cost pairs
type Pizza = (Double,Double)

-- Calculating cost per inch
costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size



-- Comparing two pizzas
comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2
                      then p1
                      else p2
   where costP1 = costPerInch p1
         costP2 = costPerInch p2

-- Describing a pizza
describePizza :: Pizza -> String
describePizza (size,cost) = "The " ++ show size ++ " pizza " ++
                            "is cheaper at " ++
                            show costSqInch ++
                            " per square inch"
   where costSqInch = costPerInch (size,cost)


-- Putting all of your code together in main'
main' :: IO ()
main' = do
   putStrLn "What is the size of pizza 1"
   size1 <- getLine
   putStrLn "What is the cost of pizza 1"
   cost1 <- getLine
   putStrLn "What is the size of pizza 2"
   size2 <-  getLine
   putStrLn "What is the cost of pizza 2"
   cost2 <- getLine
   let pizza1 = (read size1, read cost1)
   let pizza2 = (read size2, read cost2)
   let betterPizza = comparePizzas pizza1 pizza2
   putStrLn (describePizza betterPizza)


-- costData Map containing pizza cost info
costData :: Map.Map Int Double
costData = Map.fromList [(1,18.0),(2,16.0)]

-- sizeData Map containing pizza size info
sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1,20.0),(2,15.0)]

-- maybeMain: a version of your previous main using Maybe instead of IO
-- maybeMain -- > Just "The 20.0 pizza is cheaper at 5.729577951308232e-2 per square inch"
maybeMain :: Maybe String
maybeMain = do
   size1 <- Map.lookup 1 sizeData
   cost1 <- Map.lookup 1 costData
   size2 <- Map.lookup 2 sizeData
   cost2 <- Map.lookup 2 costData
   let pizza1 = (size1,cost1)
   let pizza2 = (size2,cost2)
   let betterPizza = comparePizzas pizza1 pizza2
   return  (describePizza betterPizza)

-- Q21.1

-- Translate listing 21.1 (reproduced below) into code by using do-notation in a Maybe. 
-- Assume that all the user input is replaced with a Map with a value for the input. 
-- Ignore the first putStrLn and simply return the statement at the end.

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

sampleMap :: Map.Map Int String
sampleMap = Map.fromList [(1,"Will"), (2,"Pete"),(3,"Bob")]

mainMaybe :: Maybe String
mainMaybe = do
   name <- Map.lookup 3 sampleMap
   let statement = helloPerson name
   return statement

-- Q21.2

-- Create a program that asks the user to input a number and then returns 
-- the nth Fibonacci numbers (see lesson 8 for an example of computing Fibonacci numbers).

--Fast fibonacci with momization
memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)

-- duplicate main above
main'' :: IO ()
main'' = do
   putStrLn "Enter a non-negative integer: "
   inputjar <- getLine
   let n = read inputjar:: Int
   let fib = memoized_fib n
   putStrLn$show fib
   

