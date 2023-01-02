{- ORMOLU_DISABLE -}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileSystemSpec (fileSystemSpec) where

import Data.Monoid (All (..), Any (..))
import Instances
import FileSystem
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop, modifyMaxSuccess)
import Test.QuickCheck ( (==>), Property )
import Utils
import Data.Maybe (mapMaybe, isJust)
import Data.List (intercalate)
import Control.Applicative (ZipList(..))
import Test.QuickCheck.Property ()

fileSystemSpec :: Spec
fileSystemSpec = describe "FileSystem" do
  strToPathSpec
  getFileNameSpec
  getFileContentSpec
  rewriteFileContentSpec
  getDirNameSpec
  getDirChildrenSpec

strToPathSpec :: Spec
strToPathSpec = describe "strToPath" do
  prop "correctness via pwd" \(s :: String) ->
    pwd (strToPath s)
      `shouldBe` s
  it "some test" $ strToPath "/" `shouldBe` ["/"]

getFileNameSpec :: Spec
getFileNameSpec = describe "getFileName" do
  prop "correctly returns value" \(f@(RegularFile nm _) :: RegularFile) ->
    getFileName f
      `shouldBe` nm

getFileContentSpec :: Spec
getFileContentSpec = describe "getFileContent" do
  prop "correctly returns value" \(f@(RegularFile _ con) :: RegularFile) ->
    getFileContent f
      `shouldBe` con

rewriteFileContentSpec :: Spec
rewriteFileContentSpec = describe "rewriteFileContent" do
  prop "rewrite contents properly" \(s :: String) f@(RegularFile nm _) ->
    rewriteFileContent s f
      `shouldBe` RegularFile nm s

getDirNameSpec :: Spec
getDirNameSpec = describe "getDirName" do
  prop "correctly return dir name" \dir@(Dir nm _) ->
    getDirName dir
      `shouldBe` nm

getDirChildrenSpec :: Spec
getDirChildrenSpec = describe "getDirChildren" do
  prop "correctly return the dir children" \dir@(Dir _ children) ->
    getDirChildren dir
      `shouldBe` children

-- findSpec :: Spec
-- findSpec = describe "find" do
--   prop "correctly return file with type F" \path F tree ->
--     find path F tree
--       `shouldSatisfy` isJust
--   prop "correctly return dir with type D" \path D tree ->
--     find path D tree
--       `shouldSatisfy` isJust
--   prop "correctly return same tree if path is empty" \([]::String) t tree ->
--     find [] t tree 
--       `shouldBe` Just tree
