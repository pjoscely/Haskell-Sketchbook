-- https://www.youtube.com/watch?v=t1e8gqXLbsU&t=64s
-- What is a Monad? - Computerphile

-- Data type created in video
data Expr = Val Int | Div Expr Expr deriving (Eq, Ord, Read, Show)

-- Three examples Expr Data from video:
ex_1 = Val 1

ex_2 = Div (Val 6) (Val 2)

ex_3 = Div (Val 6)  (Div (Val 3) (Val 1))

-- first eval function from video
eval :: Expr -> Int
eval (Val n) = n
-- use `div` for integer division
eval (Div x y) = eval x `div` eval y 

-- Define a safe division
safediv :: Integral a => a -> a -> Maybe a
safediv n m = if m == 0 then Nothing else Just (n `div` m)


-- rewrite eval a 2nd time
eval' :: Expr -> Maybe Int
eval' (Val n) = Just n
eval'(Div x y) = case eval' x of 
                     Nothing -> Nothing
                     Just r -> case eval' y of
                          Nothing -> Nothing
                          Just s -> safediv r s 

-- Last iteration Rewrite the eval function, using do notation
n_eval :: Expr -> Maybe Int
n_eval (Val n) = (return n)
n_eval (Div x y) = do n <- n_eval x
                      m <- n_eval y
                      safediv n m

-- More examples: 
-- n_eval(ex_4) -> Just 8
ex_4 = Div (Val 56)  (Div (Val 21) (Val 3))

--  n_eval(ex_5) -> Nothing
ex_5 =  Div (Val 21) (Val 0)

-- Types of return and maybe
-- :t return
-- return :: Monad m => a -> m a

-- :t maybe
-- maybe :: b -> (a -> b) -> Maybe a -> b







