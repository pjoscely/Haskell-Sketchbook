--Future Learn FUNCTIONAL PROGRAMMING IN HASKELL 
--Week 3 Spell Book Generator
--Helper function
-- "zoo"
--"z is for zoo"
singleWord :: String ->String
singleWord a = take 1 a ++" is for "++a


-- Main Speller Fucntion

--speller ["abacus"] -- > "a is for abacus"
--speller [] -- > ""
--speller ["apple", "banana", "coconut"] 
-- > "a is for apple, b is for banana, and c is for coconut"
--speller ["whisky", "x-ray"]
-- > "w is for whisky, and x is for x-ray"

--Use recursion to reduce to three pattern matches
speller :: [String] -> String
speller [] = ""
speller (x:[]) = singleWord x 
speller(x:y:[])= singleWord x ++ ", and "++singleWord y
speller(x:xs) = singleWord x++ ", "++speller xs








