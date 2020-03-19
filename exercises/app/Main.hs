module Main where

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
adder = do  putStr "How many numbers? "
            inputjar <- getLine
            let n = read inputjar :: Int
            -- putStr "You wish to add "
            -- putStr (show n)
            -- putStrLn " numbers together"
-- exercise in process! 
--             addhelper n 0
--
-- addhelper :: Int -> Int -> IO ()
-- addhelper 0 total = do putStr "The total is "
--                        putStrLn (show total)
-- addhelper n total = do addend <- getLine
--                        let n
