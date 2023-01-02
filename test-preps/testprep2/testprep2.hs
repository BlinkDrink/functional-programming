---------------------------------------------
------------------Problem 1------------------
---------------------------------------------
-- >>> myMember 5 [1,2,3,4]
-- False
-- >>> myMember 4 [1,2,4,5,6,7]
-- True
myMember :: Integer -> [Integer] -> Bool
myMember x = foldr (\k rec -> x == k || rec) False

myMemberStr :: String -> [String] -> Bool
myMemberStr x = foldr (\k rec -> x == k || rec) False

-- >>> myCond 5 [0,3,1,2,4]
-- True
-- >>> myCond 4 [4,1,2,0]
-- False
myCond :: Integer -> [Integer] -> Bool
myCond n [] = True
myCond n (x : xs) = (x < n && not (myMember x xs)) && myCond n xs

-- isNPerm 3 (\x -> (3 - x) `mod` 3) → True
-- isNPerm 10 (`div` 2) → False
-- isNPerm 10 (\x -> (x + 2) `mod` 10) → True
isNPerm :: Integer -> (Integer -> Integer) -> Bool
isNPerm n f = myCond n (go n)
  where
    go :: Integer -> [Integer]
    go 0 = []
    go k = f k : go (k - 1)

-- >>> getIndependentCycle 2 2 [0,2,1] (\x -> (3 - x) `mod` 3)
-- [1,2]
getIndependentCycle :: Integer -> Integer -> [Integer] -> (Integer -> Integer) -> [Integer]
getIndependentCycle start curr [] f = [f curr]
getIndependentCycle start curr (x : xs) f =
  if f curr == start
    then [f curr]
    else f curr : getIndependentCycle start (f curr) xs f

-- >>> allCycles [0,2,1] (\x -> (3 - x) `mod` 3)
allCycles :: [Integer] -> (Integer -> Integer) -> [[Integer]]
allCycles [] f = []
allCycles (x : xs) f = getIndependentCycle x x (x : xs) f : allCycles xs f

-- maxCycle 3 (\x -> (3 - x) `mod` 3) → [1, 2]
-- maxCycle 10 (\x -> (x + 2) `mod` 10) → [0, 2, 4, 6, 8]
-- maxCycle 10 (\x -> (x + 3) `mod` 10) → [0, 3, 6, 9, 2, 5, 8, 1, 4, 7]
maxCycle :: Integer -> (Integer -> Integer) -> [Integer]
maxCycle n f = foldr (\x rec -> if length x > length rec then x else rec) [] (allCycles l f)
  where
    go 0 = []
    go k = f k : go (k - 1)
    l = go n

---------------------------------------------
------------------Problem 2------------------
---------------------------------------------

-- >>> take 5 nats
-- [0.0,1.0,2.0,3.0,4.0]
nats :: [Float]
nats = 0 : map succ nats

-- >>> take 10 (fromThenFloat 1 3)
-- [1.0,4.0,7.0,10.0,13.0,16.0,19.0,22.0,25.0,28.0]
fromThenFloat :: Float -> Float -> [Float]
fromThenFloat start step = start : fromThenFloat (start + step) step

-- >>> take 10 (movingAverage nats 3)
-- [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0]
movingAverage :: [Float] -> Integer -> [Float]
movingAverage l n = (sum (take (fromInteger n) l) / fromIntegral n) : movingAverage (drop 1 l) n

-- >>> take 10 (fromThen 1 1)
-- [1,2,3,4,5,6,7,8,9,10]
fromThen :: Integer -> Integer -> [Integer]
fromThen start step = start : fromThen (start + step) step

-- >>> take 3 (allAverages nats)
allAverages :: [Float] -> [[Float]]
allAverages l = map (movingAverage l) (fromThen 2 1) -- foldr (\x rec -> movingAverage l x : rec) [] (fromThen 2 1)

---------------------------------------------
------------------Problem 3------------------
---------------------------------------------
type Label = String

type Content = [String]

type Box = (Label, Content)

type Inventory = [Box]

inv =
  [ ("docs", ["ids", "invoices"]),
    ("pics", ["family", "funny"]),
    ("ids", ["passport"]),
    ("invoices", []),
    ("memes", []),
    ("family", ["new year", "birthday"]),
    ("funny", ["memes"])
  ]

inv3 =
  [ ("docs", ["ids"]),
    ("pics", ["family"]),
    ("ids", ["passport"]),
    ("family", ["new year", "birthday"])
  ]

inv2 =
  [ ("docs", ["ids", "invoices"]),
    ("ids", ["passport"]),
    ("family", ["new year", "birthday"]),
    ("pics", ["family"])
  ]

-- isBox :: Inventory -> Label -> Bool
-- isBox [] _ = False
-- isBox ((boxName, boxContent) : boxes) name = name == boxName || isBox boxes name

isBox :: [Label] -> Label -> Bool
isBox [] _ = False
isBox (x : xs) label = label == x || isBox xs label

-- >>> getBoxNames inv
getBoxNames :: [(a, b)] -> [a]
getBoxNames [] = []
getBoxNames ((boxName, _) : xs) = boxName : getBoxNames xs

-- >>> allObjects inv
allObjects :: Inventory -> Content
allObjects myInv = go myInv
  where
    go [] = []
    go ((_, inside) : xs) = filter (not . isBox (getBoxNames myInv)) inside ++ go xs

needsCleanup :: Inventory -> Bool
needsCleanup [] = False
needsCleanup ((_, []) : xs) = True
needsCleanup ((_, children) : xs) = needsCleanup xs

cleanContent :: Label -> Inventory -> Inventory
cleanContent label [] = []
cleanContent label inven@(box@(boxName, content) : xs)
  | myMemberStr label content =
      (boxName, go label content) : cleanContent label xs
  | boxName == label && null content = cleanContent label xs
  | otherwise = box : cleanContent label xs
  where
    go _ [] = []
    go lab (k : ks) = if lab == k then go lab ks else k : go lab ks

cleanUp :: Inventory -> Inventory
cleanUp = go
  where
    go :: Inventory -> Inventory
    go [] = []
    go currInv
      | needsCleanup currInv = go (cleanContent label currInv)
      | otherwise = currInv
      where
        label = if null nextInOrder then "" else fst $ head nextInOrder
        nextInOrder = [x | x <- currInv, null $ snd x]

---------------------------------------------
------------------Problem 4------------------
---------------------------------------------

myMin :: [Integer] -> Integer
myMin [] = 0
myMin l = foldr min (head l) l

-- >>> ix 1 [1,2,3,4,5]
ix :: Integer -> [Integer] -> Integer
ix _ [] = 0
ix ind (x : xs) =
  if ind == 0
    then x
    else ix (ind - 1) xs

-- areThreeArithemtic:: Integer -> Integer -> Integer -> Bool
-- areThreeArithemtic a b c =

-- forestFire :: [Integer]
-- forestFire = foldr (\n rec -> myMin [x | x <- take (n/2) progression, x <= n / 2] ++ rec) [] progression
--   where
--     progression = fromThen 1 1

---------------------------------------------
------------------Problem 5------------------
---------------------------------------------

type Purchase = (String, String, Float)

getStore (store, _, _) = store

getCategory (_, categ, _) = categ

getPrice (_, _, price) = price

purchases :: [(String, String, Float)]
purchases =
  [ ("Billa", "Cereal", 6.50),
    ("Billa", "Vegetables", 3.5),
    ("Kaufland", "Cereal", 6),
    ("Billa", "Milk", 5),
    ("Kaufland", "Milk", 5),
    ("Kaufland", "Cereal", 10.50),
    ("Billa", "Milk", 4.5)
  ]

storeDistribution :: [Purchase] -> [(String, Float, String)]
storeDistribution [] = []
storeDistribution purchases = final
  where
    stores = unique [getStore x | x <- purchases]
    categories = unique [getCategory x | x <- purchases]
    categStorePrice categ store purchases = sum [getPrice z | z <- purchases, getStore z == store, getCategory z == categ]
    totalPricesPerCategory = unique [(x, categStorePrice x y purchases, y) | x <- categories, y <- stores, z <- purchases, y == getStore z, x == getCategory z]
    leaveMaxPurchase currCateg totalPurchases = maximum [x | x <- totalPricesPerCategory, getStore x == currCateg]
    final = [leaveMaxPurchase y totalPricesPerCategory | y <- categories]

-- categorize :: [Purchase] -> [(String, Float, String)]
-- categorize [] = []
-- categorize myStore = go myStore
--   where
--     go currCateg currStore l = foldr (\(store,categ,price) rec -> if store == currStore && categ == currCateg then ) [] l

--     removeRepetitions [] = []
--     removeRepetitions (x:xs) = if myMember x xs
--       then removeRepetitions xs
--       else x : removeRepetitions xs

--     getCategories l = removeRepetitions (foldr (\(_,categ,_) rec -> categ : rec) [] l)
--     getStores l = removeRepetitions (foldr (\(store,_,_) rec -> store : rec) [] l)

-- type Node = Char

-- type Edge = (Node, Node)

-- type Graph = ([Node], [Edge])

-- g = [('a', "bc"), ('b', "ac"), ('c', "b")]

-- graph = ("abc", [('a', 'b'), ('a', 'c'), ('b', 'e'), ('c', 'd'), ('b', 'f')])

-- convert :: [(Char, String)] -> Graph
-- convert [] = ([], [])
-- convert graph = foldr (\(nodeName, nodeNbs) (nodes, edgez) -> (nodeName : nodes, edgez ++ nodeNbs)) ([], []) (pairList graph)
--   where
--     go :: Char -> String -> [Edge]
--     go _ [] = []
--     go vertex (neighbour : ns) = (vertex, neighbour) : go vertex ns

--     pairList :: [(Char, String)] -> [(Node, [Edge])]
--     pairList [] = []
--     pairList ((v, nbs) : rest) = (v, go v nbs) : pairList rest

-- fromThen :: Integer -> Integer -> [Integer]
-- fromThen start step = start : fromThen (start + step) step

-- depthfirst :: Graph -> Node -> [Node]
-- depthfirst (v, e) n
--   | null ([x | x <- v, x == n]) = []
--   | otherwise = dfrecursive (v, e) [n]

-- dfrecursive :: Graph -> [Node] -> [Node]
-- dfrecursive ([], _) _ = []
-- dfrecursive (_, _) [] = []
-- dfrecursive (v, e) (top : stack)
--   | null [x | x <- v, x == top] = dfrecursive (newv, e) stack
--   | otherwise = top : dfrecursive (newv, e) (adjacent ++ stack)
--   where
--     adjacent = [x | (x, y) <- e, y == top] ++ [x | (y, x) <- e, y == top]
--     newv = [x | x <- v, x /= top]

-- pathsFrom :: Graph -> Vertex -> [String]
-- pathsFrom graph v = undefined

---------------------------------
------------Problem 1------------
---------------------------------

-- >>> sumOfSquareDistances (1,2) [(2,8),(-2,4),(1,2),(-4,-1),(5,0)]
-- 19.991402
sumOfSquareDistances :: (Float, Float) -> [(Float, Float)] -> Float
sumOfSquareDistances (x1, y1) pairLst = sum (map (\(x2, y2) -> sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)) pairLst)

-- >>> findMedoid [(2,8),(-2,4),(1,2),(-4,-1),(5,0)]
-- (1.0,2.0)
findMedoid :: [(Float, Float)] -> (Float, Float)
findMedoid l = foldr (\k rec -> if sumOfSquareDistances k l < sumOfSquareDistances rec l then k else rec) (head l) l

---------------------------------
------------Problem 2------------
---------------------------------

-- 3 5 [1,2,3,4,5,6,7..] -> k : [k, 2k, 4k, 8k,...]
generateToN :: Integer -> Integer -> [Integer]
generateToN _ 0 = []
generateToN k n = k : go k n
  where
    go _ 0 = []
    go k n = k : go (k * 2) (n - 1)

listN k = k : listN (k + k)

-- sumHelper [3,6,12,24,48] 93 -> 183 :
-- sumHelper  [6,12,24,48,93] 183
sumHelper list current =
  new : sumHelper (tail list ++ [new]) new
  where
    new = sum list + current - head list

-- sumLast 3 5 -> 3 ++ [3,6,12,24,48] ++ [93] :
-- sumHelper [3,6,12,24,48] 93
sumLast k n =
  [k] ++ list ++ sum list : sumHelper list (sum list)
  where
    list = take n (listN k)

-- sumLast k n = [x | x <- fromThen k 1, x == sum (take (fromIntegral (abs (n - x))) (fromThen k 1))]

---------------------------------
------------Problem 4------------
---------------------------------

data Tree = Node Integer [Tree]
  deriving (Eq, Show)

tree :: Tree
tree =
  Node
    5
    [ Node
        1
        [Node 4 [], Node 19 []],
      Node
        2
        [Node 16 [Node 12 [], Node 69 []], Node 36 []]
    ]

transformSum :: Tree -> Tree
transformSum (Node n []) = Node n []
transformSum v@(Node n children) = transformChild v
  where
    transformChild :: Tree -> Tree
    transformChild (Node val []) = Node val []
    transformChild curr@(Node val children) = Node (findSumInSubTree curr - val) (map transformChild children)

    findSumInSubTree :: Tree -> Integer
    findSumInSubTree (Node val []) = val
    findSumInSubTree (Node val successors) = val + sum (map findSumInSubTree successors)

---------------------------------
------------Problem 5------------
---------------------------------

maximumBy _ [x] = x
maximumBy func (x : xs) =
  if func x > func maxTail
    then x
    else maxTail
  where
    maxTail = maximumBy func xs

unique [] = []
unique (x : xs) = x : unique (filter (x /=) xs)

-- [[1,1,3,2],[1,1,5],[1,5],[1,1,1,3]] -> [[1],[1],[1,5],[1]]
leaveOnlyMaxRepeating :: [Integer] -> [Integer]
leaveOnlyMaxRepeating l = getMaxElements (pairsOfOccurences l)
  where
    repetitions el l = length [x | x <- l, x == el]
    pairsOfOccurences l = [(x, repetitions x l) | x <- unique l]
    getMaxElements pairs = [fst x | x <- pairs, snd x == snd (maximumBy snd pairs)]

isMemberInAll :: Integer -> [[Integer]] -> Bool
isMemberInAll n [] = False
isMemberInAll n lst = foldr ((&&) . elem n) True lst

mostFrequent :: [[Integer]] -> Integer
mostFrequent [] = 0
mostFrequent l = iterateOver (head onlyRepeating)
  where
    iterateOver :: [Integer] -> Integer
    iterateOver [] = 0
    iterateOver (x : xs) =
      if isMemberInAll x onlyRepeating
        then x
        else iterateOver xs

    onlyRepeating = map leaveOnlyMaxRepeating l

---------------------------------
------------Problem 6------------
---------------------------------

grow :: Integer -> Tree -> Tree
grow x (Node val []) = Node val [Node x [], Node x []]
grow x (Node val children) = Node val (map (grow x) children)

growButBetter :: Tree -> Tree
growButBetter (Node val []) = Node val [Node (val + 1) [], Node (val + 1) []]
growButBetter (Node val children) = Node val (map growButBetter children)

infiniteCompleteTrees :: [Tree]
infiniteCompleteTrees = Node 1 [] : map growButBetter infiniteCompleteTrees

---------------------------------
------------Problem 7------------
---------------------------------
shows = [("A", (11, 0), 120), ("B", (12, 0), 15), ("C", (10, 30), 90)]

type Name = String

type Time = (Integer, Integer)

type Duration = Integer

---------------------------------
------------Problem 7------------
---------------------------------

-- >>> ls [1,2,1]
-- [1,3,2,3,1]
ls :: [Integer] -> [Integer]
ls [] = []
ls [only] = [only]
ls (x : y : xys)
  | (not . null) xys = x : x + y : ls (y : xys)
  | otherwise = [x, x + y, y]

-- >>> stern
sternHelp :: [Integer] -> [Integer]
sternHelp curr = if curr == [1, 1] then 1 : sternHelp (ls curr) else init curr ++ sternHelp (ls curr)

stern :: [Integer]
stern = sternHelp [1, 1]
