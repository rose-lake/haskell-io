module Adder_Integer
    ( adder_integer
    ) where

import Data.Char
import Data.Digits

-- this module refactors the Example 4 code to cast to type Integer which is unbounded (!) (so cool)
-- Chapter 10, exercise 4
adder_integer :: IO ()
adder_integer = do
    n <- promptDigits "How many numbers do you wish to add together? "
    if n > 0 then do
        total <- addhelper n n 0
        putStr "The total is "
        putStrLn (show total)
    else do
        putStrLn "ERROR: Please enter a positive integer"
        adder_integer


addhelper :: Integer -> Integer -> Integer -> IO Integer
addhelper max_n 0 total = return total
addhelper max_n curr_n total = do
    let counter = (max_n - curr_n + 1)
    x <- counterPromptDigits counter " number to add > "
    addhelper max_n (curr_n-1) (total+x)


-- this version displays a prompt and fetches back an integer
promptDigits :: String -> IO Integer
promptDigits prompt = do
    putStr prompt
    input <- getLine
    if all isDigit input then
        do  let x = read input :: Integer
            return x
    else
        do  putStrLn "ERROR: Your input did not consist only of digits"
            promptDigits prompt


-- this version displays a prompt with a counter at the front, and fetches back an integer
counterPromptDigits :: Integer -> String -> IO Integer
counterPromptDigits counter prompt = do
    showCounter counter
    putStr prompt
    input <- getLine
    if all isDigit input then
        do  let x = read input :: Integer
            return x
    else
        do  putStrLn "ERROR: Your input did not consist only of digits"
            counterPromptDigits counter prompt


showCounter :: Integer -> IO ()
showCounter counter = do
    putStr (show counter)
    let list_counter = digitsRev 10 counter
    if length list_counter > 1 && list_counter !! 1 == 1 then do --this exploits Haskell lazy evaluation
        putStr "th"
    else do
        case list_counter !! 0 of
            1 -> putStr "st"
            2 -> putStr "nd"
            3 -> putStr "rd"
            _ -> putStr "th"
