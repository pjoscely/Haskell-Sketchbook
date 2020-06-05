-- Future Learn FUNCTIONAL PROGRAMMING IN HASKELL
-- Commputtaion in a simple counting system
data SimpleNum = Zero| One | Two | Many deriving (Show, Read, Eq)

--Define addition
addSimpleNum :: SimpleNum -> SimpleNum -> SimpleNum
addSimpleNum Zero Zero = Zero
addSimpleNum One Zero = One
addSimpleNum Zero One = One
addSimpleNum Zero Two = Two
addSimpleNum Two Zero = Two
addSimpleNum _ _ = Many

--Define multiplication
timesSimpleNum :: SimpleNum -> SimpleNum -> SimpleNum
timesSimpleNum Zero _ = Zero
timesSimpleNum _ Zero = Zero
timesSimpleNum One One = One
timesSimpleNum One Two = Two
timesSimpleNum Two One = Two
timesSimpleNum Many _ = Many
timesSimpleNum _ Many = Many



