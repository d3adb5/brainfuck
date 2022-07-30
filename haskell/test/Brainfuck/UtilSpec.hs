module Brainfuck.UtilSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Brainfuck

import Brainfuck.Util
import Brainfuck

spec :: Spec
spec = do
  describe "currentCell :: BF -> Word8" $ do
    prop "returns the value of the current cell" $ \bf -> do
      let currentCellValue = cells bf !! cellptr bf
      currentCell bf `shouldBe` currentCellValue

  describe "cellAt :: Int -> BF -> Word8" $ do
    prop "returns the value of the cell at a given position" $ \bf -> do
      randomIndex <- generate $ chooseInt (0, length . tail $ cells bf)
      let cellAtPositionX = cells bf !! randomIndex
      cellAt randomIndex bf `shouldBe` cellAtPositionX

  describe "withZero :: BF -> BF" $ do
    prop "turns the current cell into zero" $ \bf -> do
      currentCell (withZero bf) `shouldBe` 0

  describe "withCurrent :: Word8 -> BF -> BF" $ do
    prop "properly changes current cell" $ \bf -> do
      randomValue <- generate arbitrary
      let alteredMachine = withCurrent randomValue bf
      currentCell alteredMachine `shouldBe` randomValue

  describe "findBFLoopEnd :: String -> Int" $ do
    let for2Ps = forAll . vectorOf 2 $ sized arbitraryBFProgram
    prop "always returns the index for the matching ]" $ for2Ps $ \[p1,p2] -> do
      let fauxProgramTail = p1 ++ ']':p2
      findBFLoopEnd fauxProgramTail `shouldBe` length p1

  describe "findClosingBracketIndex :: Char -> Char -> String -> Int" $ do
    prop "always returns the index for the matching close" $ \open -> do
      let close = nextChar open
      closedRandomString <- closeBrackets open close <$> generate arbitrary
      let fauxStringTail = closedRandomString ++ [close]
      findClosingBracketIndex open close fauxStringTail `shouldBe` length closedRandomString
