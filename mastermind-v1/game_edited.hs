-- importing maps because we'll use them later; like Dictionaries in Python
import qualified Data.Map.Strict as M

-- later we compare and print these
data Color = R | B | G | Y | O | P deriving (Eq, Ord, Show)

type Secret = [Color]

type Guess = [Color]

-- data Hint = Black | White | Blank
-- type Response = [Hint]

data Response = Response
  { nBlack :: Int
  , nWhite :: Int
  , nBlank :: Int
  }


checkBlack :: Guess -> Secret -> Int
checkBlack g s = sum (map (\(a,b) -> if a == b then 1 else 0) (zip g s))

checkWhite :: Guess -> Secret -> Int
-- checkWhite g s = filter (\(a,b) -> if a == b then False else True) (zip g s)
checkWhite g s = sum $ M.elems $ uncurry (M.intersectionWith min) (foldr f acc $ filter (uncurry (/=)) (zip g s))
  -- let pairs = zip g s
  --     nonExactPairs = filter (uncurry (/=))
  --     counts = foldr f acc nonExactPairs
  --     ...
  where
    f :: (Color, Color) -> (M.Map Color Int, M.Map Color Int) -> (M.Map Color Int, M.Map Color Int)
    f (a,b) (ma, mb) =
      let ma' = case M.lookup a ma of
                  Just c -> M.insert a (c+1) ma
                  Nothing -> M.insert a 1 ma
          mb' = case M.lookup b mb of
                  Just c -> M.insert b (c+1) mb
                  Nothing -> M.insert b 1 mb
      in  (ma', mb')

    acc :: (M.Map Color Int, M.Map Color Int)
    acc = (M.empty, M.empty)

-- Used a new library: containers
--   provides Data.Map.Strict
-- We needed to learn the interface for Maps

secret :: Secret
secret = [B, Y, B, Y]

game :: IO ()
    game = do
        guess <- getGuess
        let hint1 = checkBlack secret guess
            hint2 = checkWhite secret guess

getGuess :: IO Guess
    getGuess = do
                c1 <- getChar
                c2 <- getChar
                c3 <- getChar
                c4 <- getChar
                return parseColor[parseColor c1, parseColor c2, parseColor c3, parseColor c4]

parseColor :: Char -> Color
parseColor  'R' = R
parseColor  'B' = B
parseColor  'G' = G
parseColor  'Y' = Y
parseColor  'O' = O
parseColor  'P' = P
parseColor  _ = "not a valid color!"
