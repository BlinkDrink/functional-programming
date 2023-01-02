{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import FileSystemSpec (fileSystemSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
  fileSystemSpec