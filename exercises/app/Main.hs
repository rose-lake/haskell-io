module Main where

import Data.Char
import Lib

main :: IO ()
main = do putStrLn "hi"
          putStrLn "hello world"

act :: IO (Char, Char)
act = do

        x <- getChar

        getChar
        -- note that this is the same as
        -- _ <- getChar

        y <- getChar

        -- either of these works to make things prettier:
        putStrLn ""
        -- putStr "\n"

        return (x,y)

-- Chapter 10, exercise 1
putStr' :: String -> IO ()
putStr' xs = sequence_ (fmap putChar xs)

-- Chapter 10, exercise 4
adder :: IO ()
adder = do  putStr "How many numbers do you wish to add together? "
            input <- getLine
            let n = read input :: Int
            total <- addhelper n n 0
            putStr "The total is "
            putStrLn (show total)

addhelper :: Int -> Int -> Int -> IO Int
addhelper max_n 0 total = return total
addhelper max_n curr_n total = do let nth = max_n - curr_n + 1
                                  x <- getDigit nth " number to add > "
                                  addhelper max_n (curr_n-1) (total+x)

-- -- attempt to modify getDigit helper func from Ch.10
-- -- getDigit would only fetch a single digit to convert to an integer
-- -- this function is to convert multiple-digit input to an integer
-- getInt :: Int -> String -> IO Int
-- getInt nth prompt = do  nth_str nth
--                         putStr prompt
--                         input <- getLine
--                         if (all_digits input) then
--                             let n = read input :: Int
--                             return n
--                         else
--                             do putStrLn "ERROR: Invalid digit"
--                             getInt nth prompt

-- homemade test to see if the input string consists of all digits
-- this is a LITTLE BIT of validation, but still doesn't catch the case of an input string of digits which is too long to be captured as an Int. That should still raise a run-time error / exception.
all_digits :: String -> Bool
all_digits input = (length input) == (length $ filter isDigit input)

-- slightly modified IO utility from Chapter 10
-- this version uses my 'nth_str' function to properly display the counting endings for 1st, 2nd, 3rd, etc.
getDigit :: Int -> String -> IO Int
getDigit nth prompt = do    nth_str nth
                            putStr prompt
                            x <- getChar
                            newline
                            if isDigit x then
                                return (digitToInt x)
                            else
                                do  putStrLn "ERROR: Invalid digit"
                                    getDigit nth prompt

-- my 'pretty nth output' helper func
nth_str :: Int -> IO ()
nth_str counter = do    putStr (show counter)
                        case counter of
                            1 -> putStr "st"
                            2 -> putStr "nd"
                            3 -> putStr "rd"
                            _ -> putStr "th"

-- game IO helper func from Ch.10
newline :: IO ()
newline = putChar '\n'
