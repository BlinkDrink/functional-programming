----------------------------------
------------Problem1--------------
----------------------------------

type Match = (String, String, Integer, Integer)

type Tournament = [Match]

tour = [("A", "B", 1, 0), ("B", "C", 4, 1), ("C", "B", 3, 4), ("B", "A", 1, 2), ("A", "C", 0, 1)]

tour2 = [("A", "B", 1, 0), ("B", "C", 4, 1), ("B", "C", 0, 4), ("B", "A", 1, 2), ("A", "C", 0, 1)]

-- >>> unique ["A","A","A","B","A","C","B","C"]
-- ["A","B","C"]
unique [] = []
unique (x : xs) = x : unique [k | k <- xs, k /= x]

getFirstTeam (team, _, _, _) = team

getSecondTeam (_, team, _, _) = team

getFirstScore (_, _, score, _) = score

getSecondScore (_, _, _, score) = score

calculateScore score1 score2
  | score1 > score2 = 3
  | score1 == score2 = 1
  | otherwise = 0

maximumBy :: (a -> a -> a) -> [a] -> a
maximumBy _ [] = error "Cannot pass empty list"
maximumBy _ [only] = only
maximumBy f (x : xs) = f x restMax
  where
    restMax = maximumBy f xs

myCond :: (String, Integer, Integer) -> (String, Integer, Integer) -> (String, Integer, Integer)
myCond t1@(_, points1, goalDiff1) t2@(_, points2, goalDiff2)
  | points1 > points2 = t1
  | points1 == points2 = if goalDiff1 < goalDiff2 then t1 else t2
  | otherwise = t2

getFirstOfTuple (a, _, _) = a

maxPointsMinGoal :: Tournament -> String
maxPointsMinGoal tournament = getFirstOfTuple (maximumBy myCond getAllMatches)
  where
    teams = unique [getFirstTeam x | x <- tournament] ++ [getSecondTeam x | x <- tournament]
    goalsForTeam team = sum [getFirstScore x | x <- tournament, getFirstTeam x == team] + sum [getSecondScore x | x <- tournament, getSecondTeam x == team]
    letGoalsForTeam team = sum [getSecondScore x | x <- tournament, getFirstTeam x == team] + sum [getFirstScore x | x <- tournament, getSecondTeam x == team]
    getPointsForTeam team = sum [calculateScore (getFirstScore x) (getSecondScore x) | x <- tournament, getFirstTeam x == team] + sum [calculateScore (getSecondScore x) (getFirstScore x) | x <- tournament, getSecondTeam x == team]
    getMatchesOf :: String -> [(String, Integer, Integer)]
    getMatchesOf team =
      [(team, getPointsForTeam team, goalsForTeam team - letGoalsForTeam team) | x <- tournament, getFirstTeam x == team || getSecondTeam x == team]
    getAllMatches = concat [getMatchesOf t | t <- teams]

exceedSelf tournament = exceeds allMatchPairs
  where
    teams = unique ([getFirstTeam x | x <- tournament] ++ [getSecondTeam x | x <- tournament])
    hasWonAnyAgainst team1 team2 =
      (not . null)
        ( [x | x <- tournament, getFirstTeam x == team1, getSecondTeam x == team2, getFirstScore x > getSecondScore x]
            ++ [x | x <- tournament, getFirstTeam x == team2, getSecondTeam x == team1, getSecondScore x > getFirstScore x]
        )
    teamHasWonAllAgainst team1 team2 =
      and
        ( [(\i -> getFirstScore i > getSecondScore i) x | x <- tournament, getFirstTeam x == team1, getSecondTeam x == team2]
            ++ [(\i -> getSecondScore i > getFirstScore i) x | x <- tournament, getFirstTeam x == team2, getSecondTeam x == team1]
        )
    allMatchPairs = [(getFirstTeam x, getSecondTeam x) | x <- tournament]
    exceeds ((t1, t2) : restMatches) =
      if hasWonAnyAgainst t1 t2
        then
          if and [teamHasWonAllAgainst t2 x | x <- teams, x /= t1]
            then t1
            else exceeds restMatches
        else ""

----------------------------------
------------Problem2--------------
----------------------------------

-- Дървото се представя като един Node, чиято стойност е функция от тип Integer->Bool, има списък от наследници, който може да е празен може и да не е
data Tree = Node (Integer -> Bool) [Tree]

predTree = Node (< 2) [Node odd [Node (> 6) [], Node prime []], Node (> 0) []]

prime :: Integer -> Bool
prime 1 = False
prime n = [1, n] == [x | x <- [1 .. n], n `mod` x == 0]

data Direction = L | R
  deriving (Eq, Show)

createTrace :: Tree -> Integer -> [Direction]
createTrace (Node f []) _ = []
createTrace (Node f children) n = if f n then R : createTrace (head (tail children)) n else L : createTrace (head children) n

sameTrace _ [] = False
sameTrace tree lst = go (map (createTrace tree) lst)
  where
    go [] = False
    go (dir : dirs) = elem dir dirs || go dirs

----------------------------------
------------Problem3--------------
----------------------------------

getSecondOfTuple (_, a, _) = a

getThirdOfTuple (_, _, a) = a

minimumBy :: (a -> a -> a) -> [a] -> a
minimumBy _ [] = error "Cannot pass empty list"
minimumBy _ [only] = only
minimumBy f (x : xs) = f x restMax
  where
    restMax = minimumBy f xs

calculateMinimal t1@(curr1, prev1) t2@(curr2, prev2) = if left < right then t1 else t2
  where
    left = (getFirstOfTuple curr1 - getFirstOfTuple prev1) + (getSecondOfTuple curr1 - getSecondOfTuple prev1) + (getThirdOfTuple curr1 - getThirdOfTuple prev1)
    right = (getFirstOfTuple curr2 - getFirstOfTuple prev2) + (getSecondOfTuple curr2 - getSecondOfTuple prev2) + (getThirdOfTuple curr2 - getThirdOfTuple prev2)

-- [1,2,3,4,5,6..]
-- [7,10,13,16,19,22...]
-- [14,16,18,20,22,24..]
combStreams :: [Integer] -> [Integer] -> [Integer] -> Integer -> [[Integer]]
combStreams a b c k = undefined
  where
    a' = [head a]
    b' = [head b]
    c' = [head c]
    go (x : xs) n = if n == 1 then x else go xs (n - 1)
    currSet n = [go a n, go b n, go c n]
    prevSet n = [go a' (n - 1), go b' (n - 1), go c' (n - 1)]

    genCurrTuple n = [(x, y, z) | x <- currSet n, y <- currSet n, x /= y, z <- currSet n, y /= z, z /= x]
    genPrevTuple n = [(x, y, z) | x <- prevSet n, y <- prevSet n, x /= y, z <- prevSet n, y /= z, z /= x]
    minCurrTuple n = [(curr, prev) | curr <- genCurrTuple n, prev <- genPrevTuple n]

    minTuple = minimumBy calculateMinimal (minCurrTuple k)
    final :: [Integer] -> [Integer] -> [Integer] -> [[Integer]]
    final aLst bLst cLst = [aLst ++ [getFirstOfTuple $ fst minTuple], bLst ++ [getSecondOfTuple $ fst minTuple], cLst ++ [getThirdOfTuple $ fst minTuple]]
