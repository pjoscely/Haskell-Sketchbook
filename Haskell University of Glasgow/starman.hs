--Guessing Game
--The program is going to be a guessing game, called Starman. 
--In this single-player, text-based game, there is a word which the player needs to guess. 
--For each turn of the game, the player guesses a single letter. 
--If that letter is correct, then the guessed letters are displayed 
--in the correct places in the word. If that letter is incorrect, 
--then the user loses a star. Once the user has no stars left, 
--they have lost the game. However if the user guesses all the letters in the word, 
--they have won the game.
-- Enter starman "functionally" 5 for sample play
import System.Random
check :: String -> String -> Char -> (Bool,String)
check word display c
  = (c `elem` word, [if x==c
          then c
          else y | (x,y) <- zip word display])

--Extend code to display word in case fail
turn :: String -> String -> Int -> IO ()
turn word display n =
  do if n==0
       then putStrLn ("You lose word was: " ++ word)
       else if word==display
              then putStrLn "You win!"
              else mkguess word display n

mkguess word display n =
  do putStrLn (display ++ "  " ++ take n (repeat '*'))
     putStr "  Enter your guess: "
     q <- getLine
     let (correct, display') = check word display (q!!0)
     let n' = if correct then n else n-1
     turn word display' n'

--Extension of Starman game code to generate a random word from the list
starman :: Int -> IO ()
starman n =
  do
    let words = ["hi","cat","home","country","computer","hat","wow","fly","airplane","cow","maps","make","run","sigh","golf","running","hat","television","games","hangman","pearl","cat","dog","snake","black","white","cabin","trees","birds","animals"]
    randomWord <- getStdRandom (randomR (0, length words))
    let word' = words !! randomWord
    turn word' ['-' | x <- word'] n




