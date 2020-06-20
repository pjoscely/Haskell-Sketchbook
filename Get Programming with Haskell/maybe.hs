-- Lesson 19. The Maybe type: dealing with missing values

-- *Understand the Maybe type
-- *Use the Maybe type to handle missing values
-- *Design programs with Maybe types

-- **********************************************
-- 19.1- 19.2 Introducing Maybe: solving missing values with types

import qualified Data.Map as Map
import Data.Maybe
import Data.List 

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- Map.lookup 13 organCatalog --> Just Brain

-- Map.lookup 6 organCatalog --> Nothing

-- Using Maybe as a solution to missing values

-- List of possibleDrawers in your organCatalog
possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

-- Definition of getDrawers
getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
 where getContents = \id -> Map.lookup id catalog

-- A list of availableOrgans that can contain missing values
-- availableOrgans -->
-- [Nothing,Just Heart,Nothing,Nothing,Nothing,Nothing,Just Heart,Nothing,Nothing,
-- Nothing,Nothing,Nothing,Just Brain,Just Spleen,Nothing,Nothing,Nothing,Nothing,
-- Nothing,Nothing,Just Spleen,Nothing,Nothing,Just Kidney,Nothing,Nothing,Nothing,
-- Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,
-- Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,
-- Nothing,Nothing,Nothing]

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

-- countOrgan function counts instances of an Organ
-- countOrgan Brain availableOrgans --> 1
-- countOrgan Heart availableOrgans --> 2
countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter
                                      (\x -> x == Just organ)
                                      available)

-- **********************************************
-- 19.3. Computing with Maybe


-- Definition of isSomething
--  isSomething (Just Heart) --> True
isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True

-- Using isSomething with filter to clean [Maybe Organ]
-- justTheOrgans [Just Heart,Just Heart,Just Brain,Just Spleen,Just Spleen,Just Kidney]
justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans



-- Definition of showOrgan
-- showOrgan (Just Heart) --> "Heart"
-- showOrgan Nothing --> ""
showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

-- organList --> ["Heart","Heart","Brain","Spleen","Spleen","Kidney"]
organList :: [String]
organList = map showOrgan justTheOrgans


-- cleanList --> "Heart, Heart, Brain, Spleen, Spleen, Kidney"
cleanList :: String
cleanList = intercalate ", " organList

-- Q1:

--Write a function numOrZero that takes a Maybe Int and returns 0 if it’s nothing, 
-- and otherwise returns the value.

numOrZero :: Maybe Int -> Int
numOrZero Nothing =  0
numOrZero (Just n) = n


-- ***********************************************************
-- 19.4. Back to the lab! More-complex computation with Maybe

-- Here are the rules for containers and locations:

-- For containers:

-- Brains go in a vat.
-- Hearts go in a cooler.
-- Spleens and kidneys go in a bag.

-- For locations:

-- Vats and coolers go to the lab.
-- Bags go to the kitchen.
-- You’ll start by writing this out, assuming ev

-- Defining key functions and data types for mad scientist request
data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
   show (Vat organ) = show organ ++ " in a vat"
   show (Cooler organ) = show organ ++ " in a cooler"
   show (Bag organ) = show organ ++ " in a bag"
data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location,Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

-- The core functions process and report
process :: Organ -> (Location, Container)
process organ =  placeInLocation (organToContainer organ)

report ::(Location,Container) -> String
report (location,container) = show container ++
                              " in the " ++
                              show location

-- process Brain --> (Lab,Brain in a vat)
-- process Heart --> (Lab,Heart in a cooler)
-- process Spleen --> (Kitchen,Spleen in a bag)
-- process Kidney --> (Kitchen,Kidney in a bag)
-- report (process Brain) --> "Brain in a vat in the Lab"
-- report (process Spleen) --> "Spleen in a bag in the Kitchen"


-- processAndReport to handle the Maybe Organ data
processAndReport :: (Maybe Organ) -> String
processAndReport (Just organ) = report (process organ)
processAndReport  Nothing = "error, id not found"

--processRequest with support for Maybe Organ
processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
 where organ = Map.lookup id catalog

-- processRequest 13 organCatalog --> "Brain in a vat in the Lab"
-- processRequest 12 organCatalog --> "error, id not found"

-- Q1:

-- How would you rewrite report so that it works with Maybe (Location, Container) 
-- and handles the case of the missing Organ?

report' :: Maybe (Location,Container) -> String
report' Nothing = "container not found"
report' (Just (location,container)) = show container ++
                                     " in the " ++
                                     show location


-- Q19.1

--Write a function emptyDrawers that takes the output of getDrawerContents and 
--tells you the number of drawers that are empty.
-- emptyDrawers [(Just Brain),Nothing,Nothing,(Just Spleen)]-->2
emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers contents = (length . filter isNothing) contents



-- Q19.2

-- Write a version of map that works for Maybe types, called maybeMap.
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap func Nothing = Nothing
maybeMap func (Just val) = Just (func val)







