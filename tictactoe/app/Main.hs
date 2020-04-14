module Main where

import Lib

main :: IO ()
main = someFunc --tictactoe

-- tictactoe :: IO ()
-- tictactoe = play init_board init_player
--
--
-- play :: Board -> Int -> IO ()
-- play board player = do
--     newline
--     putBoard board
--     if gameWon board then do
--         putStr "Player "
--         putStr (show (next player))
--         putStrLn " wins!!"
--     else if gameOver board then do
--         putStrLn "It is a tie."
--     else do
--         newline
--         putStr "Player "
--         putStr (show player)
--         putStr ", make your move..."
--         row <- getDigit "Enter a row number: "
--         col <- getDigit "Enter a column number: "
--         if valid board row col then
--             play (move board row col) (next player)
--         else do
--             newline
--             putStrLn "ERROR: invalid move"
--             play board player

---------------------------------
-- Game Logic
---------------------------------
valid :: Board -> Int -> Int -> Bool
valid board row col = board !! row !! col == ' '    -- verify the space is available

move :: Board -> Player -> Int -> Int -> Board
move board 1 row col = update board symbol_player_1 row col
move board 2 row col = update board symbol_player_2 row col

-- reached my first pausing point :: how to 'update' a set of symbols in Haskell?
update :: Board -> Char -> Int -> Int -> Board
update board symbol row col = []
-- 4.7.20 :: taking a detour into list comprehensions, foldr, foldl, map, fmap, etc.

---------------------------------
-- IO Game Utilities
---------------------------------
putBoard :: Board -> IO ()
putBoard [a, b, c] = do
    putStrLn "     0   1   2"
    putStrLn "   .--- --- ---."
    putRow 0 a
    putStrLn "    --- --- --- "
    putRow 1 b
    putStrLn "    --- --- --- "
    putRow 2 c
    putStrLn "   .--- --- ---."

putRow :: Int -> [Char] -> IO ()
putRow n row = do
    putStr " "
    putStr $ show n
    putStr " | "
    putChar $ row !! 0
    putStr " | "
    putChar $ row !! 1
    putStr " | "
    putChar $ row !! 2
    putStr " |\n"

newline :: IO ()
newline = do putStrLn "\n"

-----------------------------------
-- custom types and intial values
-----------------------------------
type Board = [[Char]]           -- accessed by row then column
init_row = [' ', ' ', ' ']
init_board = [init_row, init_row, init_row]
test_row_0 = ['a', 'b', 'c']
test_row_1 = ['d', 'e', 'f']
test_row_2 = ['g', 'h', 'i']
test_board = [test_row_0, test_row_1, test_row_2]

type Player = Int
init_player = 1
next :: Player -> Player
next 1 = 2
next 2 = 1

symbol_player_1 = 'o'
symbol_player_2 = 'x'
