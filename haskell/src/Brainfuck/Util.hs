module Brainfuck.Util where

import Data.Word (Word8)
import Data.List.Index (setAt)

import Brainfuck.Core

shouldLoop :: BF -> Bool
shouldLoop = (/= 0) . currentCell

currentCell :: BF -> Word8
currentCell bf = cellAt (cellptr bf) bf

cellAt :: Int -> BF -> Word8
cellAt n bf = cells bf !! n

withZero :: BF -> BF
withZero = withCurrent 0

withCurrent :: Word8 -> BF -> BF
withCurrent v bf = bf { cells = setAt (cellptr bf) v (cells bf) }

findBFLoopEnd :: String -> Int
findBFLoopEnd = findClosingBracketIndex '[' ']'

findClosingBracketIndex :: Char -> Char -> String -> Int
findClosingBracketIndex _  _  ""     = 0
findClosingBracketIndex op cl (ch:s)
  | op == ch  = newLoopSize + findClosingBracketIndex op cl skippedLoop
  | cl == ch  = 0
  | otherwise = 1 + findClosingBracketIndex op cl s
  where newLoopSize = 2 + findClosingBracketIndex op cl s
        skippedLoop = drop (newLoopSize - 1) s
