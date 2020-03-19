{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Monad
import Data.List
import System.IO
import System.Random

main :: IO ()
main = do
  secret <- replicateM 4 randomIO
  print secret
  game secret 10

game :: Secret -> Int -> IO ()
game secret turnsRemaining = do
  if turnsRemaining <= 0
  then putStrLn "you succcc"
  else do
    putStrLn "Enter a guess:"
    input <- getInput
    let (Result blacks whites) = checkCorrect secret input
    putStrLn $ "\nBlack pegs: " ++ show blacks
    putStrLn $ "White pegs: " ++ show whites
    if blacks == 4
    then do
      putStrLn "you beat meee"
    else do
      game secret (turnsRemaining - 1)

getInput :: IO Input
getInput = getInput' getChar 4

-- we don't need this anymore, after refactoring to include random secret generation
-- getSecret :: IO Input
-- getSecret = getInput' getCh 4

getInput' :: IO Char -> Int -> IO Input
getInput' myGetChar charsRemaining = do
  if charsRemaining <= 0
  then return []
  else do
    c <- myGetChar
    let color = parseColor c
    colors <- getInput' myGetChar (charsRemaining - 1)
    return (color:colors)

parseColor :: Char -> Color
parseColor 'r' = Red
parseColor 'g' = Green
parseColor 'b' = Blue
parseColor 'o' = Orange
parseColor 'y' = Yellow
parseColor 'p' = Purple
-- trying to do something like checking if it's an upper-case character... list comprehension? maybe.
-- needs work
-- parseColor [x <- ['A'..'Z']] = error "please use lowercase letters only"
parseColor _ = error "rip"

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

data Color = Red | Green | Blue | Orange | Yellow | Purple deriving Show

instance Eq Color where
    -- (==) :: Color -> Color -> Bool
    Red == Red = True
    Green == Green = True
    Blue == Blue = True
    Orange == Orange = True
    Yellow == Yellow = True
    Purple == Purple = True
    _ == _ = False

instance Random Color where
  random :: RandomGen g => g -> (Color, g)
  random gen =
    let (n, gen') = randomR (1, 6) gen
        color = colorFromInt n
    in  (color, gen')

  randomR :: RandomGen g => (Color, Color) -> g -> (Color, g)
  randomR _ gen = (Red, gen) -- too lazy

colorFromInt :: Int -> Color
colorFromInt n
  | n == 1 = Red
  | n == 2 = Orange
  | n == 3 = Yellow
  | n == 4 = Green
  | n == 5 = Blue
  | n == 6 = Purple
  | otherwise = error "rip"

data Result = Result
  { nBlacks :: Int
  , nWhites :: Int
  } deriving (Show)

type Secret = [Color]
type Input = [Color]

checkCorrect :: Secret -> Input -> Result
checkCorrect secrets input = let
    (blacks, newSecrets, nonMatches) = checkBlack secrets input
    (whites, _) = checkWhite newSecrets nonMatches
    in Result blacks whites


checkBlack :: Secret -> Input -> (Int, Secret, Input)
checkBlack [] [] = (0, [], [])
checkBlack (secret:secrets) (x:xs) = let
    (counter, newSecrets, nonMatches) = checkBlack secrets xs
    in if secret == x
    then (counter + 1, newSecrets, nonMatches)
    else (counter, secret : newSecrets, x : nonMatches)

checkWhite :: Secret -> Input -> (Int, Secret)
checkWhite secrets [] = (0, secrets)
checkWhite secrets (x:xs) = let
    (counter, newSecrets) = checkWhite secrets xs
    in if x `elem` newSecrets
        then (counter + 1, delete x newSecrets)
        else (counter, newSecrets)
