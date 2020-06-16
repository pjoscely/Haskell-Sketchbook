-- Unit 16 Q16.2

-- Create a Shape type that includes the following shapes: 
-- Circle, Square, and Rectangle. Then write a function to compute 
-- the perimeter of a Shape as well as its area.

-- Triangle perimeter and area is also included

type Radius = Double
type Height = Double
type Width = Double
type S1 = Double
type S2 = Double
type S3 = Double


data Shape = Circle Radius
           | Triangle S1 S2 S3
           | Square Height
           | Rectangle Height Width deriving Show

perimeter :: Shape -> Double
perimeter (Circle r) = 2*pi*r
perimeter (Square h) = 4*h
perimeter (Rectangle h w) = 2*h + 2*w
perimeter (Triangle a b c) = a+b+c

area :: Shape -> Double
area (Circle r) = pi*r^2
area (Square h)  = h^2
area (Rectangle h w) = h*w
area (Triangle a b c) = sqrt(s*(s-a)*(s-b)*(s-c)) where s = (a+b+c)/2