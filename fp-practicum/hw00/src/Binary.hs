module Binary where

data Binary
  = End
  | Binary :. Bit
  deriving (Show)

data Bit = Zero | One
  deriving (Show)

infixl 6 :.

succBinary :: Binary -> Binary
succBinary End = End :. One
succBinary (bin :. Zero) = bin :. One
succBinary (bin :. One) = succBinary bin :. Zero

decideLastBit :: Integer -> Bit
decideLastBit n =
  if even n
    then Zero
    else One

integerToBinary :: Integer -> Binary
integerToBinary 0 = End
integerToBinary n = integerToBinary (div n 2) :. decideLastBit n

convertBitToInt :: Bit -> Integer
convertBitToInt Zero = 0
convertBitToInt One = 1

binaryToInteger :: Binary -> Integer
binaryToInteger End = 0
binaryToInteger (bin :. bit) = 2 * binaryToInteger bin + convertBitToInt bit

hasLeadingZero :: Binary -> Bool
hasLeadingZero End = False
hasLeadingZero (End :. Zero) = True
hasLeadingZero (End :. One) = False
hasLeadingZero (bin :. _) = hasLeadingZero bin

isEnd :: Binary -> Bool
isEnd End = True
isEnd (_ :. One) = False
isEnd (bin :. Zero) = isEnd bin

canonicalise :: Binary -> Binary
canonicalise End = End
canonicalise (bin :. One) =
  if isEnd bin
    then End :. One
    else canonicalise bin :. One
canonicalise (bin :. Zero) =
  if isEnd bin
    then End
    else canonicalise bin :. Zero

addBinary :: Binary -> Binary -> Binary
addBinary End bin = bin
addBinary bin End = bin
addBinary (bin1 :. Zero) (bin2 :. Zero) = addBinary bin1 bin2 :. Zero
addBinary (bin1 :. Zero) (bin2 :. One) = addBinary bin1 bin2 :. One
addBinary (bin1 :. One) (bin2 :. Zero) = addBinary bin1 bin2 :. One
addBinary (bin1 :. One) (bin2 :. One) = addBinary (succBinary bin1) bin2 :. Zero
