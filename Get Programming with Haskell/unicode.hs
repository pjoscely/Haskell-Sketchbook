-- Lesson 23. Working with text and Unicode


-- *Use the Text type for more-efficient text processing
-- *Change Haskell’s behavior with language extensions
-- *Program by using common text functions
-- *Use Text to properly handle Unicode text
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TLazy
import qualified Data.Text.Lazy.IO as TIOLazy

-- It’s important to note that conversion isn’t computationally cheap, 
-- because you have to traverse the entire string. Avoid converting back and 
-- forth between Text and String.

-- firstWord -> "pessimism"
firstWord :: String
firstWord = "pessimism"

--  secondWord  -> "pessimism"
secondWord :: T.Text
secondWord = T.pack firstWord

-- thirdWord -> "pessimism"
thirdWord :: String
thirdWord = T.unpack secondWord

-- Throws an error
--    Couldn't match expected type ‘T.Text’ with actual type ‘[Char]’
--    In the expression: "dog"
--    In an equation for ‘myWord’: myWord = "dog"
-- myWord :: T.Text
-- myWord = "dog"

-- Using OverloadedStrings to easily assign Text using a literal
aWord :: T.Text
aWord = "Cheese"

main :: IO ()
main = do
  print aWord

-- sampleInput of type Text
sampleInput :: T.Text
sampleInput = "this\nis\ninput"

-- T.lines sampleInput
-- ["this","is","input"]

-- someText as a sample input for words
someText :: T.Text
someText = "Some\ntext for\t you"

-- T.words someText
-- ["Some","text","for","you"]

-- Code for splitOn example
breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"

-- T.splitOn breakText exampleText
-- ["This is "," to do"]

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some"," ","text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"

-- Q1:

-- Create your own version of T.lines and T.unlines by using splitOn and T.intercalate.

myLines :: T.Text -> [T.Text]
myLines text = T.splitOn "\n" text

myUnlines :: [T.Text] -> T.Text
myUnlines textLines = T.intercalate "\n" textLines

dog :: T.Text
dog = "dog"

bgText :: T.Text
bgText = "The word dog appears once in this sentence."

dharma :: T.Text
dharma = "धर्म"

dharmaT :: T.Text
dharmaT = "धर्म श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्। स्वधर्मे निधनं श्रेयः परधर्मो भयावहः"

-- Search for "dog"
-- main1 -> The word {dog} appears once in this sentence.


-- For Sanskrit 
--  main1 -> {धर्म} श्रेयान्स्व{धर्म}ो विगुणः पर{धर्म}ात्स्वनुष्ठितात्। स्व{धर्म}े निधनं श्रेयः पर{धर्म}ो भयावहः
highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
  where pieces = T.splitOn query fullText
        highlighted = mconcat ["{",query,"}"]

--  main1 -> {धर्म} श्रेयान्स्व{धर्म}ो विगुणः पर{धर्म}ात्स्वनुष्ठितात्। स्व{धर्म}े निधनं श्रेयः पर{धर्म}ो भयावहः
main1 = do
  TIO.putStrLn (highlight dharma dharmaT)

-- Q23.1

-- Rewrite the hello_world.hs program (reproduced here) from lesson 21 to use Text instead of String types.

-- Sample run
-- main23
-- Hello! What's your name?
-- Charles
-- Hello Charles!
-- helloPerson :: T.Text -> T.Text

helloPerson name = "Hello" <> " " <> name <> "!"

main23 :: IO ()
main23 = do
   TIO.putStrLn "Hello! What's your name?"
   name <- TIO.getLine
   let statement = helloPerson name
   TIO.putStrLn statement


-- Use Data.Text.Lazy and Data.Text.Lazy.IO to rewrite the lazy I/O section from 
-- lesson 22 by using the Text type.

toInts :: TLazy.Text -> [Int]
toInts = map (read.TLazy.unpack).TLazy.lines

main24 :: IO ()
main24 = do
  userInput <- TIOLazy.getContents
  let numbers = toInts userInput
  TIOLazy.putStrLn ((TLazy.pack.show.sum) numbers)


