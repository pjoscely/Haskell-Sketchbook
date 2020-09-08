{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log 

{-
Exercise 1 The first step is figuring out how to parse an individual
message. Define a function
parseMessage :: String -> LogMessage
which parses an individual line from the log file. 
For example,
parseMessage "E 2 562 help help"
== LogMessage (Error 2) 562 "help help"
cis 194: homework 2 3
parseMessage "I 29 la la la"
== LogMessage Info 29 "la la la"
parseMessage "This is not in the right format"
== Unknown "This is not in the right format"
-}
parseMessage :: String -> LogMessage
