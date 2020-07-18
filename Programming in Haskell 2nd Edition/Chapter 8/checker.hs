{-
Section 8.6 Tautology Checker 
-}

-- Declare Proposition Data type
-- Or and BiCond (<=>) are added per. Chapter 8 exercise #8
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | BiCond Prop Prop
          deriving Show

-- A Dictionary Data type 
type Assoc k v = [(k,v)]

-- Finds the first value that is associated with a given key
find :: Eq a1 => a1 -> [(a1, a2)] -> a2
find k t = head[v|(k',v) <- t, k == k']

-- Used to create a lookup table 
-- to associate variable names to logical values
type Subst = Assoc Char Bool

-- **************************************************************
-- Define propositions to test as tautologies

-- A ^ !A
p1::Prop
p1 = And(Var 'A') (Not (Var 'A'))

-- A ^ B => A
p2::Prop
p2 = Imply (And(Var 'A') (Var 'B')) (Var 'A')

-- A => (A ^ B)
p3::Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

-- (A ^ (A => B)) => B
p4::Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) ( Var 'B')

-- A Or !A
p5::Prop
p5 = Or(Var 'A') (Not (Var 'A'))

-- A <=> !A
p6::Prop
p6 = BiCond(Var 'A') (Not (Var 'A'))

-- A ^(B Or C) => (A ^ B) or (A ^ C)
p7::Prop
p7 = Imply (And (Var 'A') (Or (Var 'B')  (Var 'C'))) (Or (And (Var 'A') (Var 'B')) (And (Var 'A') (Var 'C')))
-- ****************************************************************
-- Evalutes a proposition given a substitution of its variables
eval :: [(Char, Bool)] -> Prop -> Bool

-- eval [('A',True)] (Const True) -> True
eval _ (Const b) = b 

-- eval [('B',True),('A',True),('A',False)] (Var 'A') -> True
eval s (Var x) = find x s

-- eval [('A',True)] (Not p1) -> True
eval s (Not p) = not (eval s p)

-- eval [('A',True), ('B',False)] (And (Var 'A') (Var 'B')) ->False
eval s (And p q) = eval s p && eval s q

-- eval [('A',True), ('B',False)] (Or (Var 'A') (Var 'B')) ->True
eval s (Or p q) = eval s p || eval s q

-- eval [('A',True), ('B',True)] (Imply p4 p4) ->True
eval s (Imply p q) = eval s p <= eval s q

--  eval [('A',True), ('B',False)] (BiCond p2 p3) -> False
eval s (BiCond p q) = eval s p == eval s q

-- ****************************************************************
-- Returns a list of all variables in a proposition
-- vars p2 = "ABA" == ['A', 'B','A']
vars:: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (BiCond p q)  = vars p ++ vars q
-- ****************************************************************
-- Returns a list of lists of all possible Boolean values
--  bools 3 ->
-- [[False,False,False],[False,False,True],[False,True,False],
-- [False,True,True],[True,False,False],[True,False,True],
-- [True,True,False],[True,True,True]]
bools:: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
        where bss = bools (n-1)
-- ****************************************************************
-- Removes duplicates from a list (Chapter 7)
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x:filter(/=x) (rmdups xs)
-- ****************************************************************
-- Given a proposistion generates all possible boolean 
-- substitutions for its variables
-- substs p2
-- [[('A',False),('B',False)],
-- [('A',False),('B',True)],
-- [('A',True),('B',False)],
-- [('A',True),('B',True)]]
substs:: Prop -> [Subst]
substs p = map(zip vs) (bools (length vs))
         where vs = rmdups(vars p)
-- ****************************************************************
-- Decides if a proposistion is a tautology
-- for all substitutions for its variables
-- isTaut p1 -> False
-- isTaut p2 -> True
-- isTaut p3 -> False
-- isTaut p4 -> True
-- isTaut p5 -> True
-- isTaut p6 -> False
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s<-substs p]








