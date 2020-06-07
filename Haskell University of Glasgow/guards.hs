--Guards, Guards!
--Haskell provides a notation for defining functions based on predicate values: Guards
absolute :: (Ord p, Num p) => p -> p
absolute x
  | x<0 = -x
  | otherwise = x

--Gold score
holeScore :: Int -> Int -> String
holeScore strokes par
  | score < 0 = show (abs score) ++ " under par"
  | score == 0 = "level par"
  | otherwise = show(score) ++ " over par"
 where score = strokes-par

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





