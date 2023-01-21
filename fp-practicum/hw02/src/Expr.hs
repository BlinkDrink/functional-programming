{-# LANGUAGE EmptyDataDeriving #-}

module Expr where

import Data.List (nub)
import Prelude hiding (lookup, unlines, unwords)

data Expr
  = Var String
  | Val Integer
  | Oper OperType Expr Expr
  | If Expr Expr Expr
  | SumList [Expr]
  | Sum String Expr Expr
  deriving (Show)

data OperType = Multiplication | Addition
  deriving (Show)

type Context = [(String, Integer)]

extend :: String -> Integer -> Context -> Context
extend x n = ((x, n) :)

lookup :: String -> Context -> Maybe Integer
lookup _ [] = Nothing
lookup toFind (x : xs) = if toFind == fst x then Just $ snd x else lookup toFind xs

maybeAndThen :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeAndThen mby f = f =<< mby

infixl 1 `maybeAndThen`

traverseListMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
traverseListMaybe _ [] = Just []
traverseListMaybe f (x : xs) =
  case (f x, traverseListMaybe f xs) of
    (Just x', Just xs') -> Just $ x' : xs'
    _ -> Nothing

-- (Expr If (Expr If (Var a) (Var b) (Var c)) (Expr OperType (Val a) (Var b)) (Expr Var A))

helper :: Eq a => a -> a -> a -> [a]
helper p q r
  | p == q && q == r = [p]
  | p == r && q /= p = [q]
  | p == q && p /= r = [p, r]
  | p /= q && q == r = [q]
  | otherwise = [q, r]

freeVars :: Expr -> [String]
freeVars (Val _) = []
freeVars (Var a) = [a]
freeVars (Sum [] _ _) = []
freeVars (Sum i (Val _) (Var y)) = [y | i /= y]
freeVars (Sum i (Var x) (Val _)) = if i == x then [i] else [x]
freeVars (Sum i (Var x) (Var y)) = helper i x y
freeVars (If a b c) = freeVars a ++ freeVars b ++ freeVars c
freeVars (SumList l) = concatMap freeVars l
freeVars (Oper _ left right) = freeVars left ++ freeVars right
freeVars (Sum i upper body)
  | i `elem` upperLst = allLst
  | i `elem` bodyLst = [k | k <- allLst, k /= i]
  | otherwise = allLst
  where
    upperLst = freeVars upper
    bodyLst = freeVars body
    allLst = nub (upperLst ++ bodyLst)

evalHelper :: Context -> String -> Maybe Integer
evalHelper [] _ = Nothing
evalHelper (x : xs) i = case (fst x == i, [p | p <- xs, fst p == i]) of
  (True, _) -> Just $ snd x
  (False, searched) -> case searched of
    [] -> Nothing
    x2 : _ -> Just $ snd x2

operHelper :: OperType -> Maybe Integer -> Maybe Integer -> Maybe Integer
operHelper op a b = case op of
  Multiplication -> case (a, b) of
    (Just x, Just y) -> Just $ x * y
    (_, _) -> Nothing
  Addition -> case (a, b) of
    (Just x, Just y) -> Just $ x + y
    (_, _) -> Nothing

eval :: Context -> Expr -> Maybe Integer
eval _ (Val i) = Just i
eval context (Var i) = lookup i context
eval _ (If (Val x) (Val b) (Val c)) = case x of
  0 -> Just b
  _ -> Just c
eval context (If (Val x) ex ex2) = case x of
  0 -> eval context ex
  _ -> eval context ex2
eval context (If ex body elseBody) = case eval context ex of
  Nothing -> Nothing
  Just res -> case res of
    0 -> eval context body
    _ -> eval context elseBody
eval _ (Oper op (Val a) (Val b)) = operHelper op (Just a) (Just b)
eval context (Oper op (Var a) (Val b)) = operHelper op (lookup a context) (Just b)
eval context (Oper op (Val a) (Var b)) = operHelper op (Just a) (lookup b context)
eval context (Oper op (Var a) (Var b)) = operHelper op (evalHelper context a) (lookup b context)
eval context (Oper op ex1 ex2) = operHelper op (eval context ex1) (eval context ex2)
eval context (SumList lst) = case traverseListMaybe (eval context) lst of
  Nothing -> Nothing
  Just ns -> Just $ sum ns
eval _ (Sum _ (Val a) (Val b)) = if a < 0 then Just 0 else Just $ (a + 1) * b
eval context expr@(Sum i (Var a) (Val b)) =
  if i `elem` freeVars expr
    then Nothing
    else
      ( case lookup a context of
          Nothing -> Nothing
          Just n -> if n < 0 then Just 0 else Just $ (n + 1) * b
      )
eval context (Sum i (Val a) (Var b)) =
  if i == b
    then if a < 0 then Just 0 else Just $ sum [1 .. a]
    else
      ( case lookup b context of
          Nothing -> if a < 0 then Just 0 else Nothing
          Just n -> if a < 0 then Just 0 else Just $ (a + 1) * n
      )
eval context expr@(Sum i (Var a) (Var b)) = case lookup i context of
  Nothing ->
    ( if i `elem` freeVars expr
        then Nothing
        else
          ( case (lookup a context, lookup b context) of
              (Just x, Just y) -> if x < 0 then Just 0 else (if i == b then Just $ sum [1 .. x] else Just $ (x + 1) * y)
              (_, _) -> Nothing
          )
    )
  Just iVal ->
    if i `elem` freeVars expr
      then case (lookup a context, lookup b context) of
        (Just x, Just y) -> if x < 0 then Just 0 else (if i == b then Just $ sum [1 .. iVal] else Just $ (x + 1) * y)
        (_, _) -> Nothing
      else Nothing
eval context expr@(Sum i ex1 ex2) = case lookup i context of
  Nothing ->
    if i `elem` freeVars expr
      then Nothing
      else
        ( case (eval context ex1, eval context ex2) of
            (Just x, Just y) -> Just $ (x + 1) * y
            (_, _) -> Nothing
        )
  Just _ -> case (eval context ex1, eval context ex2) of
    (Just x, Just y) -> Just $ (x + 1) * y
    (_, _) -> Nothing

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [only] = [only]
intersperse el (x : xs) = x : el : intersperse el xs

unwords :: [String] -> String
unwords l = concat $ intersperse " " l

unlines :: [String] -> String
unlines l = concat $ intersperse "\n" l

solvingCompiler :: Bool
solvingCompiler = False

newtype RacketProgram = MkRacketProgram [RacketExpr]

data RacketExpr
  = Name String
  | List [RacketExpr]
  deriving (Show)

printRacketExpr :: RacketExpr -> String
printRacketExpr (Name str) = str
printRacketExpr (List lst) = "(" ++ unwords (map printRacketExpr lst) ++ ")"

printRacketProgram :: Context -> RacketProgram -> String
printRacketProgram context (MkRacketProgram program) = unlines defined ++ "\n" ++ unlines printProgram ++ "\n"
  where
    defined = map (\(key, val) -> "(define " ++ key ++ " " ++ show val ++ ")") context
    printProgram = map printRacketExpr program

compileToRacket :: Expr -> RacketExpr
compileToRacket = undefined

solvingPartialEval :: Bool
solvingPartialEval = False

partialEval :: Expr -> Expr
partialEval = undefined

solvingSum :: Bool
solvingSum = False
