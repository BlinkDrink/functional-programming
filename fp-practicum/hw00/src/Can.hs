{-# LANGUAGE EmptyDataDeriving #-}

module Can where

import Binary (Binary (..), Bit (..), isEnd)

data LeadingOne
  = BeginOne
  | LeadingOne ::. Bit
  deriving (Show)

infixl 6 ::.

canOne :: LeadingOne
canOne = BeginOne

data Can
  = ZeroCan
  | MkCan LeadingOne
  deriving (Show)

canZero :: Can
canZero = ZeroCan

snoc :: Can -> Bit -> Can
snoc ZeroCan Zero = canZero
snoc ZeroCan One = MkCan BeginOne
snoc (MkCan leadOne) bit = MkCan $ leadOne ::. bit

forgetLeadingOne :: LeadingOne -> Binary
forgetLeadingOne BeginOne = End :. One
forgetLeadingOne (leadOne ::. bit) = forgetLeadingOne leadOne :. bit

forget :: Can -> Binary
forget ZeroCan = End
forget (MkCan BeginOne) = forgetLeadingOne BeginOne
forget (MkCan leadOne) = forgetLeadingOne leadOne

canonicalise :: Binary -> Can
canonicalise End = ZeroCan
canonicalise (End :. One) = MkCan BeginOne
canonicalise (bin :. Zero) =
  if isEnd bin
    then ZeroCan
    else snoc (canonicalise bin) Zero
canonicalise (bin :. One) =
  if isEnd bin
    then MkCan BeginOne
    else snoc (canonicalise bin) One