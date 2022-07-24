module BrainfuckSpec (spec) where

import Test.Hspec

import Data.Default (Default(..))

import Brainfuck

spec :: Spec
spec = do
  describe "data BF = BF {...}" $ do
    it "defaults to an empty machine" $ do
      shouldBe def $ BF "" 0 [] 0 [0]

  describe "next :: BF -> BF" $ do
    it "points to the next cell in an ideal situation" $ do
      let initial = BF "" 0 [] 0 [0, 0, 0]
          desired = BF "" 1 [] 1 [0, 0, 0]
      next initial `shouldBe` desired

    it "increases the number of available cells when at the end" $ do
      let initial = BF "" 0 []  9 $ replicate 10 0
          desired = BF "" 1 [] 10 $ replicate 11 0
      next initial `shouldBe` desired

  describe "prev :: BF -> BF" $ do
    it "points to the previous cell in an ideal situation" $ do
      let initial = BF "" 0 [] 2 [0, 0, 0]
          desired = BF "" 1 [] 1 [0, 0, 0]
      prev initial `shouldBe` desired

    it "increases the number of available cells when at the start" $ do
      let initial = BF "" 0 [] 0 $ replicate 10 0
          desired = BF "" 1 [] 0 $ replicate 11 0
      prev initial `shouldBe` desired

  describe "incr :: BF -> BF" $ do
    it "increments the value of the current cell" $ do
      let initial = BF "" 0 [] 2 [0, 0, 0]
          desired = BF "" 1 [] 2 [0, 0, 1]
      incr initial `shouldBe` desired

  describe "decr :: BF -> BF" $ do
    it "decrements the value of the current cell" $ do
      let initial = BF "" 0 [] 2 [0, 0, 10]
          desired = BF "" 1 [] 2 [0, 0,  9]
      decr initial `shouldBe` desired
