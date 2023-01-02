{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-dodgy-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Instances where -- (AThrice (..)) where

import Data.Monoid (All (All, getAll))
import FileSystem
import GHC.Generics (Generic)
import Generic.Random (genericArbitraryRec, genericArbitraryU, uniform, withBaseCase)
import Test.QuickCheck (Arbitrary (..), CoArbitrary, oneof, resize)
import Test.QuickCheck.Arbitrary (vector)
import Test.QuickCheck.Function
import Test.QuickCheck.Gen
import Text.Printf (printf)
import Utils

deriving instance Generic FileType

instance Arbitrary FileType where
  arbitrary = genericArbitraryU

deriving instance Generic RegularFile

instance Arbitrary RegularFile where
  arbitrary = genericArbitraryU

instance Arbitrary FileTree where
  arbitrary =
    let genRegularFile :: Gen FileTree
        genRegularFile = do
          -- fmap File arbitrary
          f <- arbitrary
          pure $ File f
        genDir :: Gen FileTree
        genDir = do
          -- fmap Directory arbitrary
          f <- arbitrary
          pure $ Directory f
     in frequency [(50, genRegularFile), (50, genDir)]

instance Arbitrary Dir where
  arbitrary = do
    s <- arbitrary
    tree <- scale (`div` 2) arbitrary
    pure (Dir s tree)
