module Main where

import Lib
import Data.Char        -- needed for digitToInt function and isDigit function

main :: IO ()
main = tictactoe

tictactoe :: IO ()
tictactoe = play init_board init_player

play :: Board -> Int -> IO ()
play board player = do
    newline
    putBoard board
    if gameWon board (symbol (next player)) then do
        newline
        putStr "  *** HOORAY *** Player "
        putStr (show (next player))
        putStrLn " wins!! *** HUZZAH ***"
        newline
    else if gameOver board then do
        newline
        putStrLn "  __ It is a tie __"
        newline
    else do
        newline
        putStr "Player "
        putStr (show player)
        putStrLn ", make your move..."
        row <- getDigit "Enter a row number: "
        col <- getDigit "Enter a column number: "
        if in_bounds row col then
            if available board row col then
                play (update board (symbol player) row col) (next player)
            else do
                newline
                putStrLn "  * ERROR: That space is taken! *"
                play board player
        else do
            newline
            putStrLn "  * ERROR: Invalid row or column *"
            play board player

---------------------------------
-- Game Logic
---------------------------------

-- check if row and col are in bounds
in_bounds :: Int -> Int -> Bool
in_bounds row col = row > (-1) && row < 3 && col > (-1) && col < 3

-- check if space at (row, col) is empty
available :: Board -> Int -> Int -> Bool
available board row col = board !! row !! col == ' '

-- update space at (row, col) with player's symbol
update :: Board -> Symbol -> Int -> Int -> Board
update [row0, row1, row2] symbol 0 col = [updateRow row0 symbol col, row1, row2]
update [row0, row1, row2] symbol 1 col = [row0, updateRow row1 symbol col, row2]
update [row0, row1, row2] symbol 2 col = [row0, row1, updateRow row2 symbol col]
update [row0, row1, row2] symbol _ col = error "Invalid row number given. Did you validate the board before updating it?"

updateRow :: Row -> Symbol -> Int -> Row
updateRow row symbol col = take col row ++ [symbol] ++ drop (col+1) row

---------------------------------
-- only check whether the board is full; not checking for a stalemate
gameOver :: Board -> Bool
gameOver [row0, row1, row2] = not $ elem ' ' row0 || elem ' ' row1 || elem ' ' row2

---------------------------------
-- all the logic to check if a game has been won
gameWon :: Board -> Symbol -> Bool
gameWon board symbol = checkColumns board symbol || checkRows board symbol || checkDiagonals board symbol

checkColumns :: Board -> Symbol -> Bool
checkColumns board symbol = checkCol 0 board symbol || checkCol 1 board symbol || checkCol 2 board symbol

checkCol :: Int -> Board -> Symbol -> Bool
checkCol col [row0, row1, row2] symbol = (row0 !! col) == symbol && (row1 !! col) == symbol && (row2 !! col) == symbol

checkRows :: Board -> Symbol -> Bool
checkRows [row0, row1, row2] symbol = checkRow row0 symbol || checkRow row1 symbol || checkRow row2 symbol

checkRow :: Row -> Symbol -> Bool
checkRow row symbol = (row !! 0) == symbol && (row !! 1) == symbol && (row !! 2) == symbol

checkDiagonals :: Board -> Symbol -> Bool
checkDiagonals board symbol = checkDiagonalPos board symbol || checkDiagonalNeg board symbol

-- Pos as in Positive as in positive 'slope'
checkDiagonalPos :: Board -> Symbol -> Bool
checkDiagonalPos [row0, row1, row2] symbol = (row2 !! 0) == symbol && (row1 !! 1) == symbol && (row0 !! 2) == symbol

-- Neg as in Negative as in negative 'slope'
checkDiagonalNeg :: Board -> Symbol -> Bool
checkDiagonalNeg [row0, row1, row2] symbol = (row0 !! 0) == symbol && (row1 !! 1) == symbol && (row2 !! 2) == symbol

---------------------------------
-- IO Game Utilities
---------------------------------
-- um, yes, I am 'row' and 'column' directionally challenged,
-- so, as a favor to myself and other future players,
-- I've included indicators for 'row' and 'column' on the displayed game board... ;-)
putBoard :: Board -> IO ()
putBoard [a, b, c] = do
    putStrLn "        column"
    putStrLn "      0   1   2"
    putStrLn "    .--- --- ---."
    putRow 0 a
    putStrLn "r    --- --- --- "
    putRow 1 b
    putStrLn "w    --- --- --- "
    putRow 2 c
    putStrLn "    .--- --- ---."

putRow :: Int -> Row -> IO ()
putRow n row = do
    if n == 1 then
        do putStr "o"
    else
        do putStr " "
    putStr " "
    putStr $ show n
    putStr " | "
    putChar $ row !! 0
    putStr " | "
    putChar $ row !! 1
    putStr " | "
    putChar $ row !! 2
    putStr " |\n"

-- gets a single digit of input from the user
getDigit :: String -> IO Int
getDigit prompt = do
    putStrLn prompt
    x <- getChar
    newline
    if isDigit x then
        return (digitToInt x)
    else do
        newline
        putStrLn "\t* ERROR: Invalid digit *"
        getDigit prompt

newline :: IO ()
newline = do putStr "\n"

-----------------------------------
-- custom types and intial values
-----------------------------------
type Board = [[Char]]           -- accessed by row then column
type Row = [Char]
init_row = [' ', ' ', ' ']
init_board = [init_row, init_row, init_row]
-- for testing board display
test_row_0 = ['a', 'b', 'c']
test_row_1 = ['d', 'e', 'f']
test_row_2 = ['g', 'h', 'i']
test_board = [test_row_0, test_row_1, test_row_2]
-- for testing wins
o_0 = ['x', 'x', 'o']
o_1 = [' ', 'o', ' ']
o_2 = ['o', ' ', ' ']
o_win = [o_0, o_1, o_2]
x_0 = ['x', 'x', 'o']
x_1 = [' ', 'x', ' ']
x_2 = ['o', ' ', 'x']
x_win = [x_0, x_1, x_2]

type Player = Int
init_player = 1
next :: Player -> Player
next 1 = 2
next 2 = 1

type Symbol = Char
symbol_player1 :: Symbol
symbol_player1 = 'o'
symbol_player2 :: Symbol
symbol_player2 = 'x'
symbol :: Player -> Symbol
symbol 1 = symbol_player1
symbol 2 = symbol_player2
-- first, try without this code
-- next_symbol :: Symbol -> Symbol
-- next_symbol symbol_player1 = symbol_player2
-- next_symbol symbol_player2 = symbol_player1
