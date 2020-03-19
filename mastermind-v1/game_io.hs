import qualified Data.Map.Strict as Map
-- Used a new library: containers
--   provides Data.Map.Strict
-- We needed to learn the interface for Maps

import Control.Monad

data Color = R | B | G | Y | O | P deriving (Eq, Ord, Show)
-- note: the reason this derives Eq and Ord is in order to use it as a key in the Data.Map.Strict

type Secret = [Color]

type Guess = [Color]

-- data Hint = Black | White | Blank
-- type Response = [Hint]

-- data Response = Response
--   { nBlack :: Int
--   , nWhite :: Int
--   , nBlank :: Int
--   }

checkBlack :: Guess -> Secret -> Int
checkBlack g s = sum (map (\(a,b) -> if a == b then 1 else 0) (zip g s))

checkWhite :: Guess -> Secret -> Int
checkWhite g s = sum $ Map.elems $ uncurry (Map.intersectionWith min) (foldr f acc $ filter (uncurry (/=)) (zip g s))
  where
    f :: (Color, Color) -> (Map.Map Color Int, Map.Map Color Int) -> (Map.Map Color Int, Map.Map Color Int)
    f (a,b) (ma, mb) =
      let ma' = case Map.lookup a ma of
                  Just c -> Map.insert a (c+1) ma
                  Nothing -> Map.insert a 1 ma
          mb' = case Map.lookup b mb of
                  Just c -> Map.insert b (c+1) mb
                  Nothing -> Map.insert b 1 mb
      in  (ma', mb')
    acc :: (Map.Map Color Int, Map.Map Color Int)
    acc = (Map.empty, Map.empty)

secret :: Secret
secret = [B,Y,B,Y]

start :: IO ()
start = game 6

game :: Int -> IO ()
game 0 = putStrLn "you lost"
game count = do
  guess <- getGuess
  let hint1 = checkBlack guess secret
      hint2 = checkWhite guess secret
  putStrLn $ "\n" ++ show hint1 ++ " black pegs and " ++ show hint2 ++ " white pegs."

  -- if checkBlack guess secret == 4 then do
  -- also, you can just reuse your local variable hint2 ::
  if hint1 == 4 then do
    putStrLn "u beat meee"
  else game (count-1)

-- getGuess :: IO Guess
-- getGuess = do
--   c1 <- getChar
--   c2 <- getChar
--   c3 <- getChar
--   c4 <- getChar
--   return [parseColor c1, parseColor c2, parseColor c3, parseColor c4]

-- getGuess :: IO Guess
-- getGuess = do
--   chars <- replicateM 4 getChar
--   return $ map parseColor chars

getGuess :: IO Guess
getGuess = replicateM 4 (fmap parseColor getChar)

-- getChar :: IO Char
-- fmap (here) :: (Char -> Color) -> IO Char -> IO Color
-- replicateM :: Int -> IO Color -> IO [Color]

parseColor :: Char -> Color
parseColor 'R' = R
parseColor 'B' = B
parseColor 'G' = G
parseColor 'Y' = Y
parseColor 'O' = O
parseColor 'P' = P
parseColor _ = error "not a valid color choice!"
