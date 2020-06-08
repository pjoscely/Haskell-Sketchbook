--Guards, Guards!
--Haskell provides a notation for defining functions based on predicate values: Guards
absolute :: (Ord p, Num p) => p -> p
absolute x
  | x<0 = -x
  | otherwise = x

--Golf score
holeScore :: Int -> Int -> String
holeScore strokes par
  | score < 0 = show (abs score) ++ " under par"
  | score == 0 = "level par"
  | otherwise = show(score) ++ " over par"
 where score = strokes-par

--Taken from http://learnyouahaskell.com/
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  

--Case statement
--hello (Parrot "polly")
data Pet = Cat | Dog | Fish | Parrot String
hello :: Pet -> String
hello x = 
  case x of
    Cat -> "meeow"
    Dog -> "woof"
    Fish -> "bubble"
    Parrot name -> "pretty " ++ name





