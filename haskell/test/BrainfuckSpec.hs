{-# LANGUAGE TupleSections #-}

module BrainfuckSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (output)
import Test.QuickCheck.Instances.Tuple ((>*<), (>**<))

import Control.Monad.IO.Class (liftIO)
import Data.Char (chr)
import Data.Default (Default(..))
import Data.List (sort)
import Data.List.Index (setAt)
import Data.Word (Word8)
import System.IO.Fake (fakeIO)

import Brainfuck

instance Arbitrary BF where
  arbitrary = do
    let progGen n = arbitraryBFProgram (n + 1)
        lstkGen n = chooseInt (0, div n 2) >>= \ln -> sort <$> vectorOf ln (chooseInt (0, n))
        cellGen n = vectorOf (n + 1) arbitrary
        ctrGen n = chooseInt (0, n)
    (prog, pctr, lstk) <- sized $ \pn -> (>**<) (progGen pn) (ctrGen pn) (lstkGen pn)
    (clls, cctr) <- sized $ \cn -> cellGen cn >*< ctrGen cn
    pure $ BF prog pctr lstk cctr clls

arbitraryBFProgram :: Int -> Gen String
arbitraryBFProgram n = normalize <$> vectorOf n (elements "+-[]<>,.")
  where normalize p = let (c,o) = loopWrappers 0 0 p in
          replicate c '[' ++ p ++ replicate o ']'

loopWrappers :: Int -> Int -> String -> (Int, Int)
loopWrappers c o [] = (c, o)
loopWrappers c 0 (']':p) = loopWrappers (c + 1) 0 p
loopWrappers c o (']':p) = loopWrappers c (o - 1) p
loopWrappers c o ('[':p) = loopWrappers c (o + 1) p
loopWrappers c o ( _ :p) = loopWrappers c o p

findLoopEnd :: String -> Int
findLoopEnd "" = 0
findLoopEnd (']':_) = 1
findLoopEnd ('[':cs) =
  let newEnd = findLoopEnd cs
  in 1 + newEnd + findLoopEnd (drop newEnd cs)
findLoopEnd (_:cs) = 1 + findLoopEnd cs

currentCell :: BF -> Word8
currentCell m = cells m !! cellptr m

spec :: Spec
spec = do
  describe "data BF = BF {...}" $ do
    it "defaults to an empty machine" $ do
      shouldBe def $ BF "" 0 [] 0 [0]

  describe "next :: BF -> BF" $ do
    prop "moves the pointer to the next cell" $ \bf ->
      cellptr (next bf) `shouldBe` 1 + cellptr bf

    prop "increases the number of available cells when at the end" $ \bf -> do
      let numberOfCells = length $ cells bf
          pointerAtEnd = bf { cellptr = numberOfCells - 1 }
          pointerMoved = next pointerAtEnd
      length (cells pointerMoved) `shouldBe` 1 + numberOfCells

  describe "prev :: BF -> BF" $ do
    prop "moves the pointer to the previous cell" $ \bf ->
      cellptr (prev bf) `shouldBe` max 0 (cellptr bf - 1)

    prop "increases the number of available cells when at the start" $ \bf -> do
      let pointerAtStart = bf { cellptr = 0 }
          numberOfCells = length $ cells bf
          pointerMoved = prev pointerAtStart
      length (cells pointerMoved) `shouldBe` 1 + numberOfCells

  describe "incr :: BF -> BF" $ do
    prop "increments the value of the current cell" $ \bf ->
      currentCell (incr bf) `shouldBe` 1 + currentCell bf

  describe "decr :: BF -> BF" $ do
    prop "decrements the value of the current cell" $ \bf ->
      currentCell (decr bf) `shouldBe` currentCell bf - 1

  describe "lbeg :: BF -> BF" $ do
    prop "pushes next index to loop stack if current cell is non-zero" $ \bf -> do
      let nonzero | currentCell bf == 0 = (incr bf) { progctr = progctr bf }
                  | otherwise = bf
      head (loopind $ lbeg nonzero) `shouldBe` 1 + progctr bf

    prop "skips to loop close if current cell is zero" $ \bf -> do
      let closedp = program bf ++ "]"
          zeroedc = setAt (cellptr bf) 0 (cells bf)
          machine = bf { program = closedp, cells = zeroedc }
          lclosei = 1 + progctr machine + findLoopEnd premain
          premain = drop (1 + progctr machine) $ program machine
          lbegged = lbeg machine
      progctr lbegged `shouldBe` lclosei
      program lbegged !! (progctr lbegged - 1) `shouldBe` ']'

  describe "lend :: BF -> BF" $ do
    prop "applies index from loop stack if current cell is non-zero" $ \bf -> do
      let nestack = nonzero { loopind = loopind bf ++ [0] }
          nonzero | currentCell bf == 0 = (incr bf) { progctr = progctr bf }
                  | otherwise = bf
          lended = lend nestack
      progctr lended `shouldBe` head (loopind nestack)
      loopind lended `shouldBe` loopind nestack

    prop "pops index from loop stack if current cell is zero" $ \bf -> do
      let zeroedm = bf { cells = setAt (cellptr bf) 0 (cells bf) }
          nestack = zeroedm { loopind = loopind bf ++ [0] }
          lended = lend nestack
      progctr lended `shouldBe` 1 + progctr bf
      loopind lended `shouldBe` tail (loopind nestack)

  describe "cput :: BF -> IO BF" $ do
    prop "reads character from stdin into cell" $ \bf -> do
      (machine, _) <- liftIO $ fakeIO (cput bf) "A"
      currentCell machine `shouldBe` 65

    prop "reads null character into cell when hitting EOF" $ \bf -> do
      (machine, _) <- liftIO $ fakeIO (cput bf) ""
      currentCell machine `shouldBe` 0

  describe "cget :: BF -> IO BF" $ do
    prop "puts character from cell into stdout" $ \bf -> do
      (machine, output) <- liftIO $ fakeIO (cget bf) ""
      progctr machine `shouldBe` 1 + progctr bf
      head output `shouldBe` chr (fromEnum $ currentCell bf)
