--XOR: The magic of cryptography.

-- xorBool, a foundation for xor
xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))

-- xorPair to xor tuple pairs of Bools
-- xorPair (True, True)
-- False

xorPair :: (Bool,Bool) -> Bool
xorPair (v1,v2) = xorBool v1 v2


-- Completed xor function on a pair of Bool lists --> lists
-- xor [True, False]  [True, False] --> [False,False]
xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)


-- Bits type synonym
type Bits = [Bool]

-- Helper function to convert an Int into Bits
-- intToBits' 31 --> [True,True,True,True,True]
-- Notice the list is reversed
-- intToBits' 16 --> [False,False,False,False,True]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (remainder == 0)
               then False : intToBits' nextVal
               else True : intToBits' nextVal
 where remainder = n `mod` 2
       nextVal = n `div` 2

-- Use in intToBits Below
maxBits :: Int
maxBits = length (intToBits' maxBound)

-- This corrects the reversed list from intToBits'
-- Also produces a large list of 63 items
-- intToBits 255
-- [False,False,False,False,False,False,False,False,False,False,False,
-- False,False,False,False,False,False,False,False,False,False,False,False,
-- False,False,False,False,False,False,False,False,False,False,False,False,
-- False,False,False,False,False,False,False,False,False,False,False,False,
-- False,False,False,False,False,False,False,False,True,True,True,True,True,
-- True,True,True]
intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
   where reversedBits = reverse  (intToBits' n)
         missingBits = maxBits - (length reversedBits)
         leadingFalses = take missingBits (cycle [False]) 

-- Helper fucntion 
-- fromEnum 'c' --> 99 --> 63 element list of Bits
charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

-- Returns the integer value of the a Bits list
-- bitsToInt $ charToBits 'c' --> 99
bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
 where size = length bits
       indices = [size-1,size-2 .. 0]
       trueLocations = filter (\x -> fst x == True)
                       (zip bits indices)


-- bitsToChar (charToBits 'a') -->'a'
bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

-- Build a one-time pad cipher

-- A simple pad
myPad :: String
myPad = "Shhhhhh"

-- plain text
myPlainText :: String
myPlainText = "Haskell"

-- To encrypt your myPlainText, you convert both your pad and 
-- your plain text to bits, and then xor the results.

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext =  map (\pair ->
                                 (fst pair) `xor` (snd pair))
                           (zip padBits plaintextBits)
 where padBits =  map charToBits pad
       plaintextBits =  map charToBits plaintext


--applyOTP to encode strings using a one-time pad
-- GHCi> applyOTP myPad myPlainText
-- "\ESC\t\ESC\ETX\r\EOT\EOT"
applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
 where bitList = applyOTP' pad plaintext


--Partial application to create an encoderDecoder
-- GHCi> encoderDecoder "book"
-- "1\a\a\ETX"
-- GHCi> encoderDecoder "1\a\a\ETX"
-- "book"
encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad


-- *************************************************

-- A Cipher class to generalize operations

class Cipher a where
   encode :: a -> String -> String
   decode :: a -> String -> String


-- The OneTimePad data type
data OneTimePad = OTP String

-- Making OneTimePad an instance of Cipher
instance Cipher OneTimePad where
   encode (OTP pad) text = applyOTP pad text
   decode (OTP pad) text = applyOTP pad text


-- Using lazy evaluation to create a limitless pad
myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

-- GHCi> encode myOTP "Learn Haskell"
-- "Ldcqj%Nf{bog`"
-- GHCi> decode myOTP "Ldcqj%Nf{bog`"
-- "Learn Haskell"
-- GHCi> encode myOTP "this is a longer sentence, I hope it encodes"
-- "tikp$lu'i)fdbjk}0bw}`pxt}5:R<uqoE\SOHKW\EOT@HDGMOX"
-- GHCi> decode myOTP "tikp$lu'i)fdbjk}0bw}`pxt}5:R<uqoE\SOHKW\EOT@HDGMOX"
-- "this is a longer sentence, I hope it encodes"

-- **************************************************
-- A linear congruential PRNG
prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a*seed + b) `mod` maxNumber

-- Given a seed will generate a psuedorandom Int from 0..4
examplePRNG :: Int -> Int
examplePRNG  = prng 1337 7 5

-- To do:
-- use the PRNG to create a StreamCipher type.



