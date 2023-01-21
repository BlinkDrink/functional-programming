module TicTacToe where

import Data.Maybe (isNothing)
import Matrix

data Marker = X | O
  deriving (Eq, Show)

type Spot = Maybe Marker

type Board = Matrix Spot

data Result = Full | HasEmpty | Wins Marker
  deriving (Show)

infixr 2 `join`

isWinner :: Result -> Bool
isWinner (Wins _) = True
isWinner _ = False

join :: Result -> Result -> Result
join HasEmpty HasEmpty = HasEmpty
join HasEmpty (Wins x) = Wins x
join res Full = res
join Full res = res
join resWin res =
  if not (isWinner res) && isWinner resWin
    then resWin
    else res

checkThreeSpots :: Thrice Spot -> Result
checkThreeSpots thr =
  case threeRow of
    (Just X, Just X, Just X) -> Wins X
    (Just O, Just O, Just O) -> Wins O
    (Nothing, _, _) -> HasEmpty
    (_, Nothing, _) -> HasEmpty
    (_, _, Nothing) -> HasEmpty
    (_, _, _) -> Full
  where
    threeRow = thriceToTriple thr

winnerRows :: Board -> Result
winnerRows (MkMatrix m) = foldThriceWith join $ \i -> checkThreeSpots (m i)

winnerCols :: Board -> Result
winnerCols m = foldThriceWith join $ \i -> checkThreeSpots (getCol i m)

winnerDiags :: Board -> Result
winnerDiags m = join (checkThreeSpots (getDiag m)) (checkThreeSpots (getOtherDiag m))

winner :: Board -> Result
winner m = join (join (winnerRows m) (winnerCols m)) (winnerDiags m)

emptySpots :: Board -> [(Three, Three)]
emptySpots m = foldMatrixWith (++) (imapMatrix (\i j e -> [(i, j) | isNothing e]) m)
