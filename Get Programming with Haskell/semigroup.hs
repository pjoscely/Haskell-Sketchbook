-- Lesson 17. Design by composition—Semigroups and Monoids

-- Create new functions with function composition
-- Use Semigroup to mix colors
-- Learn how to use guards in code
-- Solve probability problems with Monoid
-- ************************************************

-- 17.1. Intro to composability—combining functions

import Data.List
import Data.Semigroup
import Data.Monoid

-- returns the last element of a list
myLast :: [a] -> a
myLast = head . reverse

-- returns the min element of a list
myMin :: Ord a => [a] -> a
myMin = head . sort

-- returns the max element of a list
myMax :: Ord a => [a] -> a
myMax = myLast . sort

-- tests if a property is true for all items in a list
-- testFunc x = x > 0
-- myAll testFunc [1,2,3,4]
-- True
myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = (foldr (&&) True) . (map testFunc)

-- myAny tests that a property is True for at least one value in the list.
-- testFunc x = x > 0
-- myAny testFunc [(-1),(-2),(-3)]
-- False

-- myAny even [1,2,3]
-- True
myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) False) . (map testFunc)

-- ************************************************
-- 17.2. Combining like types: Semigroups

-- The Semigroup class has only one important method you need, the <> operator. 
-- You can think of <> as an operator for combining instances of the same type. 
-- You can trivially implement Semigroup for Integer by defining <> as +.

-- Semigroup for Integer
-- :t (<>)
-- (<>) :: Semigroup a => a -> a -> a
-- (<>) 2 3 --> 5

instance Semigroup Integer where
   (<>) x y = x + y

-- You can easily use types to represent this problem of mixing colors.
data Color = Red |
   Yellow |
   Blue |
   Green |
   Purple |
   Orange |
   Brown deriving (Show,Eq)


 -- Implementing Semigroup for Color version 1
 -- Red <> Yellow --> Orange
instance Semigroup Color where
   (<>) Red Blue = Purple
   (<>) Blue Red = Purple
   (<>) Yellow Blue = Green
   (<>) Blue Yellow = Green
   (<>) Yellow Red = Orange
   (<>) Red Yellow = Orange
   (<>) a b = if a == b
              then a
              else Brown

-- VERSION 1 IS NOT ASSOCIATIVE 
-- *** (Green <> Blue) <> Yellow --> Brown
-- *** Green <> (Blue <> Yellow) --> Green

-- Use abreviations and reimplement Semigroup for Color to support associativity

data C = R |
   Y |
   B |
   G |
   P |
   O |
   Br deriving (Show,Eq)


instance Semigroup C where
   (<>) R B = P
   (<>) B R = P
   (<>) Y B = G
   (<>) B Y = G
   (<>) Y R = O
   (<>) R Y = O
   (<>) a b | a == b = a
            | all (`elem` [R,B,P]) [a,b] = P
            | all (`elem` [B,Y,G]) [a,b] = G
            | all (`elem` [R,Y,O]) [a,b] = O
            | otherwise = B

-- Version 2 is now Associative
-- (G <> B) <> Y --> G
-- G <> (B <> Y) --> G

-- ************************************************
-- 17.3. Combining like types: Semigroups
-- The only major difference between Semigroup and Monoid is that Monoid 
-- requires an identity element for the type.

-- The actual definition of Monoid
-- class Monoid a where
 -- mempty :: a
 -- mappend :: a -> a -> a
 -- mconcat :: [a] -> a

-- -- ************************************************
-- 17.3.3. Practical Monoids—building probability tables

-- Type synonyms for Events and Probs
type Events = [String]
type Probs = [Double]

-- PTable data type
data PTable = PTable Events Probs


-- Create PTable makes a PTable ensuring all probabilities sum to 1
createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
 where totalProbs = sum probs
       normalizedProbs = map (\x -> x/totalProbs) probs

-- showPair creates a String for a single event-probability pair
showPair :: String -> Double -> String
showPair event prob = mconcat [event,"|", show prob,"\n"]


-- Making PTable an instance of Show
instance Show PTable where
   show (PTable events probs) = mconcat pairs
     where pairs = zipWith showPair events probs

-- Example:
-- createPTable ["heads","tails"] [0.5,0.5]
-- heads|0.5
-- tails|0.5

-- The cartCombine function for the Cartesian product of lists
cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
 where nToAdd = length l2
       repeatedL1 = map (take nToAdd . repeat) l1
       newL1 = mconcat repeatedL1
       cycledL2 = cycle l2

-- combineEvents and combineProbs
combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
 where combiner = (\x y -> mconcat [x,"-",y])

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

-- Making PTable an instance of Semigroup
instance Semigroup PTable where
   (<>) ptable1 (PTable [] []) = ptable1
   (<>) (PTable [] []) ptable2 = ptable2
   (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
     where newEvents = combineEvents e1 e2
           newProbs = combineProbs p1 p2

--  Making PTable an instance of Monoid
instance Monoid PTable where
   mempty = PTable [] []
   mappend = (<>)

-- Example PTables coin and spinner
coin :: PTable
coin = createPTable ["heads","tails"] [0.5,0.5]

spinner :: PTable
spinner = createPTable ["red","blue","green"] [0.1,0.2,0.7]

-- GHCi> coin <> spinner
-- heads-red|5.0e-2
-- heads-blue|0.1
-- heads-green|0.35
-- tails-red|5.0e-2
-- tails-blue|0.1
-- tails-green|0.35


-- GHCi> mconcat [coin,coin,coin]
-- heads-heads-heads|0.125
-- heads-heads-tails|0.125
-- heads-tails-heads|0.125
-- heads-tails-tails|0.125
-- tails-heads-heads|0.125
-- tails-heads-tails|0.125
-- tails-tails-heads|0.125
-- tails-tails-tails|0.125


-- Q17.1

-- Your current implementation of Color doesn’t contain an identity element. 
-- Modify the code in this unit so that Color does have an identity element, 
-- and then make Color an instance of Monoid.

data Color1 = Red1 |
   Yellow1 |
   Blue1 |
   Green1 |
   Purple1 |
   Orange1 |
   Brown1 |
   Clear deriving (Show,Eq)

instance Semigroup Color1 where
   (<>) Clear any = any
   (<>) any Clear = any
   (<>) Red1 Blue1 = Purple1
   (<>) Blue1 Red1= Purple1
   (<>) Yellow1 Blue1 = Green1
   (<>) Blue1 Yellow1 = Green1
   (<>) Yellow1 Red1 = Orange1
   (<>) Red1 Yellow1 = Orange1
   (<>) a b | a == b = a
            | all (`elem` [Red1,Blue1,Purple1]) [a,b] = Purple1
            | all (`elem` [Blue1,Yellow1,Green1]) [a,b] = Green1
            | all (`elem` [Red1,Yellow1,Orange1]) [a,b] = Orange1
            | otherwise = Brown1

instance Monoid Color1 where
   mempty = Clear
   mappend col1 col2 = col1 <> col2


-- Q17.2

-- If your Events and Probs types were data types and not just synonyms, 
-- you could make them instances of Semigroup and Monoid, where combineEvents and 
-- combineProbs were the <> operator in each case. Refactor these types and 
-- make instances of Semigroup and Monoid.

-- Name change to avoid conflict with already defined Events and Probs
data  Evts = Evts [String]
data  Prbs = Prbs [Double]

combineEvnts :: Evts -> Evts -> Evts
combineEvnts (Evts e1) (Evts e2) = Evts (cartCombine combiner e1 e2)
  where combiner = (\x y -> mconcat [x,"-",y])

instance Semigroup Evts where
  (<>) = combineEvnts

instance Monoid Evts where
  mappend = (<>)
  mempty = Evts []

combinePrbs :: Prbs -> Prbs -> Prbs
combinePrbs (Prbs p1) (Prbs p2) = Prbs (cartCombine (*) p1 p2)

instance Semigroup Prbs where
  (<>) = combinePrbs

instance Monoid Prbs where
  mappend = (<>)
  mempty = Prbs []  
