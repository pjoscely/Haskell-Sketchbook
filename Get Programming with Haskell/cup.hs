--In this exercise, you’ll see how to use the tools of functional programming 
--to replicate common design features found in OOP languages. 
--We will build a simple cup object.

--Constructor for a basic cup object
cup :: t1 -> (t1 -> t2) -> t2
cup flOz = \message -> message flOz


--Takes a cup object and returns the number of fluid ounces (flOz) it has.
--An accessor
getOz :: ((p -> p) -> t) -> t
getOz aCup = aCup (\flOz -> flOz)


drink:: (Ord t1, Num t1) => ((p -> p) -> t1) -> t1 -> (t1 -> t2) -> t2
drink aCup ozDrank = if ozDiff >= 0
                     then cup ozDiff
                     else cup 0
  where flOz = getOz aCup
        ozDiff = flOz - ozDrank


--Helper message to check whether the cup is empty.
isEmpty :: (Eq a, Num a) => ((p -> p) -> a) -> Bool
isEmpty aCup = getOz aCup == 0

-- *********** Sample run in ghci **************

-- coffeeCup = cup 20
-- isEmpty coffeeCup
-- False

-- afterManySips = foldl drink coffeeCup [1,1,1,1,1]
-- getOz afterManySips
-- 15

-- empty = drink coffeeCup 20
-- getOz empty
-- 0


