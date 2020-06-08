--Monads allow sequencing of function calls to be enforced by the type system. 

--A monad has three building blocks:

-- *** 1 ***
--A type constructor that produces the type of a computation, 
--given the type of that computation’s result.

-- *** 2 ***
--A function that takes a value, and returns a computation that—when 
--executed—will produce that value.

-- *** 3 ***
--A function that takes two computations and performs them one after the other, 
--making the result of the first computation available to the second.

--IO monad wrapped up as a do block.
hello :: String -> IO String
hello x =
  do
     putStrLn ("Hello, " ++ x)
     putStrLn "What's your name?"
     name <- getLine
     return name
    

--Example: the Maybe monad


-- Example use of Maybe: Safe head and tail
myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x:xs) = Just x

myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail (x:xs) = Just xs

--This safely returns the third element of a list
baz :: [a] -> Maybe a
baz xs =
      do  a <- myTail xs
          b <- myTail a
          c <- myHead b
          return c



