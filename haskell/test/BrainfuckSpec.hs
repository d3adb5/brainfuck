module BrainfuckSpec (spec) where

import Test.Hspec

import Control.Monad.IO.Class (liftIO)
import Data.Default (Default(..))
import System.IO.Fake (fakeIO)

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

  describe "lbeg :: BF -> BF" $ do
    it "pushes next index to loop stack if current cell is non-zero" $ do
      let initial = BF "" 0 [ ] 0 [1, 0, 0]
          desired = BF "" 1 [1] 0 [1, 0, 0]
      lbeg initial `shouldBe` desired

    it "pushes -1 to the loop stack if current cell is zero" $ do
      let initial = BF "" 0 [  ] 0 [0, 0, 0]
          desired = BF "" 1 [-1] 0 [0, 0, 0]
      lbeg initial `shouldBe` desired

  describe "lend :: BF -> BF" $ do
    it "applies index from loop stack if current cell is non-zero" $ do
      let initial = BF "" 100 [50, 20] 1 [0, 1, 0]
          desired = BF ""  50 [50, 20] 1 [0, 1, 0]
      lend initial `shouldBe` desired

    it "pops index from loop stack if current cell is zero" $ do
      let initial = BF "" 100 [50, 20] 1 [0, 0, 0]
          desired = BF "" 101 [20]     1 [0, 0, 0]
      lend initial `shouldBe` desired

    it "pops -1 from loop stack and continues regardless of current cell value" $ do
      let initialN = BF "" 100 [-1, 20] 1 [0, 1, 0]
          desiredN = BF "" 101 [20]     1 [0, 1, 0]
          initialZ = BF "" 123 [-1, 25] 1 [0, 0, 0]
          desiredZ = BF "" 124 [25]     1 [0, 0, 0]
      lend initialN `shouldBe` desiredN
      lend initialZ `shouldBe` desiredZ

  describe "cput :: BF -> IO BF" $ do
    it "reads character from stdin into cell" $ do
      let initial = BF "" 0 [] 1 [0,  0, 0]
          desired = BF "" 1 [] 1 [0, 65, 0]
      (machine,_) <- liftIO $ fakeIO (cput initial) "A"
      machine `shouldBe` desired

  describe "cget :: BF -> IO BF" $ do
    it "puts character from cell into stdout" $ do
      let initial = BF "" 0 [] 1 [0, 65, 0]
          desired = BF "" 1 [] 1 [0, 65, 0]
      (machine, output) <- liftIO $ fakeIO (cget initial) ""
      machine `shouldBe` desired
      output `shouldBe` "A"
