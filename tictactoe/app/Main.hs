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
    if gameWon board then do
        putStr "Player "
        putStr (show (next player))
        putStrLn " wins!!"
    else if gameOver board then do
        putStrLn "It is a tie."
    else do
        newline
        putStr "Player "
        putStr (show player)
        putStrLn ", make your move..."
        row <- getDigit "Enter a row number: "
        col <- getDigit "Enter a column number: "
        if valid board row col then
            play (move board player row col) (next player)
        else do
            newline
            putStrLn "  * ERROR: Invalid move *"
            play board player

---------------------------------
-- Game Logic
---------------------------------

gameWon :: Board -> Bool
gameWon _ = False

-- gameOver is a check whether the board is full
-- not currently checking for a stalemate, which can occur before the board is full
-- simply check if there are any spaces present in any row
gameOver :: Board -> Bool
gameOver _ = False

-- Note: before calling 'move' always call 'valid', first, to ensure that the row and col are in bounds and the space is available.
move :: Board -> Player -> Int -> Int -> Board
move board 1 row col = update board symbol_player1 row col
move board 2 row col = update board symbol_player2 row col

update :: Board -> Symbol -> Int -> Int -> Board
update board symbol 0 col = [updateRow (board !! 0) symbol col,
                             board !! 1,
                             board !! 2]
update board symbol 1 col = [board !! 0,
                             updateRow (board !! 1) symbol col,
                             board !! 2]
update board symbol 2 col = [board !! 0,
                             board !! 1,
                             updateRow (board !! 2) symbol col]
update board symbol _ col = error "Invalid row number given. Are you sure you called 'valid' before calling 'udapte' on the current Board?"

updateRow :: Row -> Symbol -> Int -> Row
updateRow row symbol col = take col row ++ [symbol] ++ drop (col+1) row

-- Note: in the main game loop, first we check if a move is valid, then we call 'move'
valid :: Board -> Int -> Int -> Bool
valid board row col =
    if in_bounds row col then
        board !! row !! col == ' '    -- verify the space is available
    else
        False

-- Note: throughout this game we are simply hard-coding (including with the initial board setup) the fact that the board is of size 3 x 3
in_bounds :: Int -> Int -> Bool
in_bounds row col = row > (-1) && row < 3 && col > (-1) && col < 3

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
test_row_0 = ['a', 'b', 'c']
test_row_1 = ['d', 'e', 'f']
test_row_2 = ['g', 'h', 'i']
test_board = [test_row_0, test_row_1, test_row_2]
a_game_0 = ['x', ' ', 'o']
a_game_1 = [' ', 'o', ' ']
a_game_2 = ['x', ' ', ' ']
a_game = [a_game_0, a_game_1, a_game_2]

type Player = Int
init_player = 1
next :: Player -> Player
next 1 = 2
next 2 = 1

type Symbol = Char
symbol_player1 = 'o'
symbol_player2 = 'x'
