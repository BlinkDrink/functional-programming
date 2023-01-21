{-# LANGUAGE LambdaCase #-}

module Matrix where

data Three = Zero | One | Two
  deriving (Eq, Show)

flipThree :: Three -> Three
flipThree Zero = Two
flipThree One = One
flipThree Two = Zero

type Thrice a = Three -> a

thriceToTriple :: Thrice a -> (a, a, a)
thriceToTriple t = (t Zero, t One, t Two)

thrice :: a -> a -> a -> Thrice a
thrice x y z t =
  case t of
    Zero -> x
    One -> y
    Two -> z

newtype Matrix a = MkMatrix {getMatrix :: Thrice (Thrice a)} -- Three -> Three -> a

matrix ::
  a ->
  a ->
  a ->
  a ->
  a ->
  a ->
  a ->
  a ->
  a ->
  Matrix a
matrix x0 x1 x2 y0 y1 y2 z0 z1 z2 =
  MkMatrix $ \case
    Zero -> thrice x0 x1 x2
    One -> thrice y0 y1 y2
    Two -> thrice z0 z1 z2

-- MkMatrix $ \case
--   ...cases...
-- is exactly the same as
-- MkMatrix $ \i ->
--   case i of
--     ...cases...
-- It requires the `LambdaCase` language extension - it's enabled at the top of the file

constantMatrix :: a -> Matrix a
constantMatrix el = MkMatrix $ \_ _ -> el

diagonalMatrix :: a -> Matrix (Maybe a)
diagonalMatrix el = MkMatrix $ \row col ->
  if row == col
    then Just el
    else Nothing

otherDiagonalMatrix :: a -> Matrix (Maybe a)
otherDiagonalMatrix el = MkMatrix $ \row col ->
  if row == flipThree col
    then Just el
    else Nothing

addMatrix :: Matrix Integer -> Matrix Integer -> Matrix Integer
addMatrix (MkMatrix m1) (MkMatrix m2) = MkMatrix $ \row col -> m1 row col + m2 row col

showThrice :: (a -> String) -> Thrice a -> String
showThrice func thr = func (thr Zero) ++ " " ++ func (thr One) ++ " " ++ func (thr Two)

showMatrix :: (a -> String) -> Matrix a -> String
showMatrix func (MkMatrix m) = showThrice func (m Zero) ++ "\n" ++ showThrice func (m One) ++ "\n" ++ showThrice func (m Two)

ix :: Three -> Three -> Matrix a -> a
ix row col (MkMatrix m) = m row col

getRow :: Three -> Matrix a -> Thrice a
getRow row (MkMatrix m) = m row

getCol :: Three -> Matrix a -> Thrice a
getCol col (MkMatrix m) y = m y col

getDiag :: Matrix a -> Thrice a
getDiag (MkMatrix m) y = m y y

getOtherDiag :: Matrix a -> Thrice a
getOtherDiag (MkMatrix m) y = m y x
  where
    x = flipThree y

transpose :: Matrix a -> Matrix a
transpose (MkMatrix m) = MkMatrix $ \row col -> m col row

foldThriceWith :: (a -> a -> a) -> Thrice a -> a
foldThriceWith func thr = func (func (thr Zero) (thr One)) (thr Two)

foldMatrixWith :: (a -> a -> a) -> Matrix a -> a
foldMatrixWith op (MkMatrix m) = foldThriceWith op $ \i -> foldThriceWith op $ \j -> m i j

eqMatrix :: (a -> a -> Bool) -> Matrix a -> Matrix a -> Bool
eqMatrix func (MkMatrix m1) (MkMatrix m2) = foldMatrixWith (&&) (MkMatrix (\i j -> func (m1 i j) (m2 i j)))

imapThrice :: (Three -> a -> b) -> Thrice a -> Thrice b
imapThrice func thr y = func y (thr y)

mapThrice :: (a -> b) -> Thrice a -> Thrice b
mapThrice func = imapThrice (\_ el -> func el)

imapMatrix :: (Three -> Three -> a -> b) -> Matrix a -> Matrix b
imapMatrix func (MkMatrix m) = MkMatrix $ \i j -> func i j (m i j)

mapMatrix :: (a -> b) -> Matrix a -> Matrix b
mapMatrix func = imapMatrix (\_ _ el -> func el)

place :: Three -> Three -> a -> Matrix a -> Matrix a
place row col el = imapMatrix (\i j x -> if i == row && j == col then el else x)

concatThriceWith :: String -> Thrice String -> String
concatThriceWith delimiter thr = thr Zero ++ delimiter ++ thr One ++ delimiter ++ thr Two

concatMatrixWith :: String -> String -> Matrix String -> String
concatMatrixWith rowD colD (MkMatrix m) = concatThriceWith rowD $ \i -> concatThriceWith colD $ \j -> m i j

showMatrixComposition :: (a -> String) -> Matrix a -> String
showMatrixComposition func m = concatMatrixWith "\n" " " (mapMatrix func m)

instance Show a => Show (Matrix a) where
  show = showMatrixComposition show

{-
type GenericMatrix =

foldGenericMatrix ::
mapGenericMatrix ::
multGenericMatrix ::
-}
