{-
Exercise 5 The Towers of Hanoi is a classic puzzle with a solution
that can be described recursively. Disks of different sizes are stacked
on three pegs; the goal is to get from a starting configuration with
all disks stacked on the first peg to an ending configuration with all
disks stacked on the last peg.

*       *       *
*		*       *
*       *       *
*       *       *
*       *       *

src    aux     dest

The only rules are
• you may only move one disk at a time, and
• a larger disk may never be stacked on top of a smaller one.

For example, as the first move all you can do is move the topmost,
smallest disk onto a different peg, since only one disk may be moved
at a time.

To move n discs (stacked in increasing size) from peg a to peg b
using peg c as temporary storage,

1. move n − 1 discs from a to c using b as temporary storage
2. move the top disc from a to b
3. move n − 1 discs from c to b using a as temporary storage.

For this exercise, define a function hanoi with the following type:
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

Given the number of discs and names for the three pegs, hanoi
should return a list of moves to be performed to move the stack of
discs from the first peg to the second.
Note that a type declaration, like type Peg = String above, makes
a type synonym. In this case Peg is declared as a synonym for String,
and the two names Peg and String can now be used interchangeably.
Giving more descriptive names to types in this way can be used to
give shorter names to complicated types, or (as here) simply to help
with documentation.

-}

type Peg = String
type Move = (Peg, Peg)
--Example: hanoi 2 "a" "c" "b" -> [("a","b"),("a","c"),("b","c")]
--Example: hanoi 3 "a" "c" "b" ->
--[("a","c"),("a","b"),("c","b"),("a","c"),("b","a"),("b","c"),("a","c")]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 src dest aux = []
hanoi n src dest aux = hanoi (n-1) src aux dest ++ [(src,dest)] ++ hanoi (n-1) aux dest src
{-
Exercise 6 (Optional) What if there are four pegs instead of three?
That is, the goal is still to move a stack of discs from the first peg to
the last peg, without ever placing a larger disc on top of a smaller
one, but now there are two extra pegs that can be used as “temporary” storage instead of only one. 
Write a function similar to hanoi
which solves this problem in as few moves as possible.
It should be possible to do it in far fewer moves than with three
pegs. For example, with three pegs it takes 2^15 − 1 = 32767 moves
to transfer 15 discs. With four pegs it can be done in 129 moves. (See
Exercise 1.17 in Graham, Knuth, and Patashnik, Concrete Mathematics,
second ed., Addison-Wesley, 1994.)

The Frame-Stewart algorithm:
1. for some k (1 <= k < n), move k discs from src to aux1
2. move the remaining n-k discs from src to dst without using aux1
3. move k discs from aux1 to dst
It has been proved that for 4 pegs, the value of k that minimizes
the number of moves is the one that we have used here.
https://en.wikipedia.org/wiki/Tower_of_Hanoi#Frame%E2%80%93Stewart_algorithm
-}
-- hanoi4 3  "a" "d" "b" "c" ->
-- [("a","b"),("a","c"),("a","d"),("c","d"),("b","d")]
-- length$hanoi4 15  "a" "d" "b" "c" -> 129
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg->[Move]
hanoi4 0 _ _ _ _ = []
hanoi4 n src dst aux1 aux2 =
       hanoi4 k src aux1 aux2 dst ++ 
       hanoi (n-k) src dst aux2 ++ 
       hanoi4 k aux1 dst aux2 src
       where
       n' = fromIntegral n :: Double
       k  = n - round (sqrt (2*n' + 1)) + 1












