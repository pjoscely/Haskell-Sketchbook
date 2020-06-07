-- Finger exercises experimenting with scope

--Trip cost calculator
journeycost :: Float -> Float -> Float
journeycost miles fuelcostperlitre = 
 let milespergallon = 35
     litrespergallon = 4.55
     gallons = miles/milespergallon
 in (gallons*litrespergallon*fuelcostperlitre)

 --Circle calculator
circlecalc :: Floating a => a -> [a]
circlecalc radius =
  let diameter = 2*radius
      circumference = pi*diameter
  in  [diameter, circumference]

--Body Mass Index Formula
--height in meters weight in kilograms
bmi :: Float -> Float -> Float
bmi height weight =
  weight/(height^2)

--Convert hours to seconds
hrs_secs :: Float -> Float
hrs_secs hours =
  hours*3600

--Simple Interest = (Principal Amount * Rate of Interest * Number of years) / 100
simpleInt :: Float -> Float -> Float -> Float 
simpleInt princ rate years =
  (princ*rate*years)/100

--Compute the Euclidean distance between the points (x1, y1) and (x2, y2). 
eucliddist :: Float -> Float -> Float -> Float -> Float
eucliddist x1 y1 x2 y2 = 
  let dx = (x1-x2)^2
      dy = (y1-y2)^2
  in  sqrt(dx + dy)

--Compute the Manhattan distance between the points (x1, y1) and (x2, y2). 
manhatdist :: Float -> Float -> Float -> Float -> Float
manhatdist x1 y1 x2 y2 = 
  let dx = abs(x1-x2)
      dy = abs(y1-y2)
  in  dx + dy

--Heron's Formula for the area of a triangle
heron :: Float -> Float -> Float -> Float 
heron s1 s2 s3 =
  let s= (s1 + s2 + s3)/2
  in sqrt(s*(s-s1)*(s-s2)*(s-s3))

--The where keyword
squareplusone :: Int -> Int
squareplusone x = xsquared + 1
  where xsquared = x*x

--The where keyword
cel2fahr :: Float -> Float
cel2fahr x = (x*scalingfactor) + freezingpoint
 where scalingfactor = 9.0/5.0
       freezingpoint = 32
