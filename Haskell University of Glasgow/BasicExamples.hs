-- Future Learn FUNCTIONAL PROGRAMMING IN HASKELL
-- Week 1 More Basic Elements by Example
import Data.Map
--Quadratic Formula does not check for complex roots
roots :: Float -> Float -> Float -> [Float]
roots a b c = 
    let
        det2 = b*b-4*a*c;
        det  = sqrt(det2);
            rootp = (-b + det)/a/2;
            rootm = (-b - det)/a/2;
        in
            [rootm,rootp]

--Max function
max' :: Ord p => p -> p -> p
max' x y = 
    if x > y
        then x
        else y

--case statement
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list." 


-- Java Map
-- Map<String,Integer> set = new HashMap<String,Integer>();
--
--In Haskell 
set = Data.Map.empty 
set' = Data.Map.insert "Answer" 42 set






