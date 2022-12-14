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

cleanContent :: Label -> Inventory -> Inventory
cleanContent label [] = []
cleanContent label (box@(boxName, content) : xs)
  | myMemberStr label content =
      (boxName, go label content) : cleanContent label xs
  | boxName == label = cleanContent label xs
  | otherwise = box : cleanContent label xs
  where
    go _ [] = []
    go lab (k : ks) = if lab == k then go lab ks else k : go lab ks

-- type Tree = (Label, Content)
-- toTree :: Inventory -> Tree
-- toTree ((boxName, content):boxes) =

-- cleanUp :: Inventory -> Inventory
-- cleanUp ((boxName,content):boxes) = go myInv
--   where
--     go (boxName, []) =
--     go (boxName, content)

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

purchases = 
        [
         ("Billa",    "Cereal", 6.50),
         ("Billa",    "Milk",   5),
         ("Kaufland", "Milk",   5),
         ("Kaufland", "Cereal", 6),
         ("Kaufland", "Cereal", 10.50),
         ("Billa",    "Milk",   4.5)
         ]

categorize :: [Purchase] -> [(String, Float, String)]
categorize [] = []
categorize myStore = go myStore
  where
    go currCateg currStore l = foldr (\(store,categ,price) rec -> if store == currStore && categ == currCateg then ) [] l

    removeRepetitions [] = []
    removeRepetitions (x:xs) = if myMember x xs 
      then removeRepetitions xs
      else x : removeRepetitions xs

    getCategories l = removeRepetitions (foldr (\(_,categ,_) rec -> categ : rec) [] l)
    getStores l = removeRepetitions (foldr (\(store,_,_) rec -> store : rec) [] l)
