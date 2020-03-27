module Main where

import Lib
import Adder_Int
import Adder_Integer
import Examples

main :: IO ()
main = do
    putStrLn "Here is 'adder_integer' :: it will add a specified number of integers together for you!"
    adder_integer
    putStrLn "Use ghci to access and play with various other functions and examples I've implemented!"
