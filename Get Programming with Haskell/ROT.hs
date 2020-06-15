-- Lesson 15. Capstone: Secret messages!
-- This capstone covers
-- *Learning about the basics of cryptography
-- *Using basic types to model your data
-- *Making practical use of Enum and Bounded
-- *Writing and making instances of your own Cipher class

-- Defining a four-letter alphabet
data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show,Enum,Bounded)

fourLetterMessage :: [FourLetterAlphabet]
fourLetterMessage = [L1,L3,L4,L1,L1,L2]

-- A generic rotN function to work on any alphabet 
-- similiar to the Caesar cipher ROT13 on the standard 26 letter alphabet
-- RotN RotN gives the identity function

-- Example in ghci
-- GHCi> rotN 4 L1
-- L3
-- GHCi> rotN 4 L2
-- L4
-- GHCi> rotN 4 L3
-- L1
-- GHCi> rotN 4 L4
-- L2

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
 where halfAlphabet = alphabetSize `div` 2
       offset = fromEnum c + halfAlphabet
       rotation =  offset `mod` alphabetSize


-- Define a three letter alphabet 

data ThreeLetterAlphabet = Alpha
                          | Beta
                          | Kappa deriving (Show,Enum,Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha,Alpha,Beta,Alpha,Kappa]


-- Create a similar function to rotN, which adds 1 to the offset if the alphabet has an odd number of letters.
-- An issue arises when decoding odd-sized alphabets because 
-- you’re doing integer division and always rounding down.
-- The code below corrects for this
rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
 where halfN = n `div` 2
       offset = if even n
                then fromEnum c + halfN
                else 1 + fromEnum c + halfN
       rotation =  offset `mod` n

-- Finally, you can put this all together to create a robust rotEncoder and rotDecoder 
-- to decode strings. These will work even if a single extra Char is added or removed, 
-- making the number of Char letters odd.

-- GHCi> fourLetterEncoder fourLetterMessage
-- [L3,L1,L2,L3,L3,L4]
-- GHCi> fourLetterDecoder(fourLetterEncoder fourLetterMessage)
-- [L1,L3,L4,L1,L1,L2]

-- GHCi> threeLetterMessage
-- [Alpha,Alpha,Beta,Alpha,Kappa]
-- GHCi> threeLetterEncoder threeLetterMessage
-- [Beta,Beta,Kappa,Beta,Alpha]
-- GHCi> threeLetterDecoder (threeLetterEncoder threeLetterMessage)
-- [Alpha,Alpha,Beta,Alpha,Kappa]

-- GHCi> rotEncoder "hi"
-- "\557160\557161"
-- GHCi> rotDecoder(rotEncoder "hi")
-- "hi"
-- GHCi> rotEncoder "Jean-Paul likes Simone"
-- "\557130\557157\557153\55....
-- GHCi> rotDecoder (rotEncoder "Jean-Paul likes Simone")
-- "Jean-Paul likes Simone"

rotEncoder :: String -> String
rotEncoder text = map rotChar text
 where alphaSize = 1 + fromEnum (maxBound :: Char)
       rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder text =  map rotCharDecoder text
 where alphaSize = 1 + fromEnum (maxBound :: Char)
       rotCharDecoder = rotNdecoder alphaSize

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals =  map rot3l vals
 where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
       rot3l = rotN alphaSize

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals =  map rot3ldecoder vals
 where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
       rot3ldecoder = rotNdecoder alphaSize

fourLetterEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterEncoder vals = map rot4l vals
 where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
       rot4l = rotN alphaSize

fourLetterDecoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterDecoder vals =  map rot4ldecoder vals
 where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
       rot4ldecoder = rotNdecoder alphaSize


