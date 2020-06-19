-- Lesson 18. Parameterized types

-- *Use parameterized types to make generic data types
-- *Understand kinds of types
-- *Write code using the Data.Map type to look up values

import Data.Char
import qualified Data.Map as Map
-- ************************************
-- 18.1. Types that take arguments

-- Example of Box parametrized type

data Box a = Box a deriving Show

-- The Box type is an abstract container that can hold any other type. 
-- As soon as you put a type inside Box, the Box type takes on a concrete value. 

-- GHCi> n = 6 :: Int
-- GHCi> :t Box n
-- Box n :: Box Int
-- GHCi> word = "box"
-- GHCi> :t Box word
-- Box word :: Box [Char]
-- GHCi> f x = x
-- GHCi> :t Box f
-- Box f :: Box (t -> t)
-- GHCi> otherBox = Box n
-- GHCi> :t Box otherBox
-- Box otherBox :: Box (Box Int)

-- You can also make simple functions for your Box, such as wrap and unwrap 
-- to put items into or take them out of a box.

-- Defining the wrap and unwrap functions for Box
wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

-- What’s the type of wrap (Box 'a')?
-- Box (Box Char)

-- Defining the Triple type
data Triple a = Triple a a a deriving Show

-- Defining a 3D point in space as a Triple
type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

-- Using a Triple to define a name data type
type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

-- Using a Triple to define Initials
type Initials = Triple Char

initials :: Initials
initials = Triple 'H' 'P' 'L'

-- Assessors for the Triple type
first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _ ) = x

third :: Triple a -> a
third (Triple _ _ x) = x


-- Defining a toList function on Triple
toList :: Triple a -> [a]
toList (Triple x y z) = [x,y,z]


-- A function to transform Triples
transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)


-- transform (* 3) aPoint
-- Triple 0.30000000000000004 159.60000000000002 36.900000000000006

-- transform reverse aPerson
-- Triple "drawoH" "spillihP" "tfarcevoL"

-- transform toLower initials
-- Triple 'h' 'p' 'l'

-- toList (transform toLower initials)
-- "hpl"

-- Defining your own list
data List a = Empty | Cons a (List a) deriving Show

-- Comparing your List to the built-in list
builtinEx1 :: [Int]
builtinEx1 = 1:2:3:[]

ourListEx1 :: List Int
ourListEx1 = Cons 1 (Cons 2 (Cons 3 Empty))

builtinEx2 :: [Char]
builtinEx2 = 'c':'a':'t':[]

ourListEx2 :: List Char
ourListEx2 = Cons 'c' (Cons 'a' (Cons 't' Empty))


-- Defining ourMap for your list
ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap func (Cons a rest)  = Cons (func a) (ourMap func rest)

-- ourMap (*2) ourListEx1
-- Cons 2 (Cons 4 (Cons 6 Empty))

-- *****************************************
-- 18.2. Types with more than one parameter
-- Definition of a tuple
-- data (,) a b = (,) a b

-- Exploring the types of tuples
itemCount1 :: (String,Int)
itemCount1 = ("Erasers",25)

itemCount2 :: (String,Int)
itemCount2 = ("Pencils",25)

itemCount3 :: (String,Int)
itemCount3 = ("Pens",13)

-- Creating an item inventory
-- itemInventory
--[("Erasers",25),("Pencils",25),("Pens",13)]
itemInventory :: [(String,Int)]
itemInventory = [itemCount1,itemCount2,itemCount3]




-- Data.Map

-- The Organ data type
data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)

-- An example list of organs
organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

-- A map looks up values by using a binary search tree. 
-- This is slower than a hash table but still fast. 
-- The map looks up values by searching the keys needed 
-- to have the property of being of class Ord, so you can 
-- compare two keys and efficiently find them in the tree.

-- a type can have a type !!!!!!!!!!!

-- The kind of a type indicates the number of parameters the type takes, 
-- which are expressed using an asterisk (*). Types that take no parameters have a kind of *, 
-- types that take one parameter have the kind * -> *, 
-- types with two parameters have the kind * -> * -> *, and so forth.

-- GHCi> :kind Int
-- Int :: *
-- GHCi> :kind Triple
-- Triple :: * -> *
-- GHCi> :kind []
-- [] :: * -> *
-- GHCi> :kind (,)
-- (,) :: * -> * -> *
-- GHCi> :kind Map.Map
-- Map.Map :: * -> * -> *


--  A List of IDs to represent the locations of various organs
ids :: [Int]
ids = [2,7,13,14,21,24]

-- organPairs created using zip
-- organPairs --> [(2,Heart),(7,Heart),(13,Brain),(14,Spleen),(21,Spleen),(24,Kidney)]
organPairs :: [(Int,Organ)]
organPairs = zip ids organs

-- Creating your organCatalog
-- Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a


organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- Map.lookup 7 organCatalog --> Just Heart
-- Map.lookup 1  organCatalog -- >Nothing


-- Q18.1

-- For the types Triple and Box, implement a function similar to map, tripleMap, and boxMap.
-- inc x = x +1
-- boxMap inc(Box 2)
-- Box 3
boxMap :: (a -> b) -> Box a -> Box b
boxMap func (Box val) = Box (func val)


-- inc x = x+1
-- tripleMap inc(Triple 2 3 4)
-- Triple 3 4 5
tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap func (Triple v1 v2 v3) = Triple (func v1) (func v2) (func v3)


-- Q18.2

-- Modify the Organ type so that it can be used as a key. 
-- Then build a Map, organ-Inventory, of each organ to its 
-- count in the organCatalog.

-- The trick is that Organ needs to be of type Ord to be a key for a Map.
-- add enum to easily build a list of all organs:
-- data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq,Enum)
--values --> [Heart,Heart,Brain,Spleen,Spleen,Kidney]
values :: [Organ]
values = map snd (Map.toList organCatalog)

allOrgans :: [Organ]
allOrgans = [Heart .. Spleen]

-- organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]
-- organCounts --> [2,1,1,2]
organCounts :: [Int]
organCounts = map countOrgan allOrgans
  where countOrgan = (\organs ->
                       (length . filter (== organs)) values)

-- organInventory --> fromList [(Heart,2),(Brain,1),(Kidney,1),(Spleen,2)]
organInventory :: Map.Map Organ Int
organInventory = Map.fromList (zip allOrgans organCounts)

