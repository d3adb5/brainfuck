module Brainfuck (
    BF(..),
    next, prev,
    incr, decr
  ) where

import Data.Default

data BF = BF { program :: String
             , progctr :: Int
             , loopind :: [Int]
             , cellptr :: Int
             , cells :: [Int]
             } deriving (Show, Eq)

instance Default BF where
  def = BF "" 0 [] 0 [0]

next :: BF -> BF
next m = m { cellptr = 1 + cellptr m, cells = adjusted, progctr = 1 + progctr m }
  where adjusted = take (max (2 + cellptr m) cellspan) $ cells m ++ repeat 0
        cellspan = length $ cells m

prev :: BF -> BF
prev m = m { cellptr = newptr, cells = adjusted, progctr = 1 + progctr m }
  where adjusted = if cellptr m == 0 then 0 : cells m else cells m
        newptr = max 0 (cellptr m - 1)

incr :: BF -> BF
incr m = m { cells = modified, progctr = 1 + progctr m }
  where modified = modifyAt (cellptr m) (+ 1) (cells m)

decr :: BF -> BF
decr m = m { cells = modified, progctr = 1 + progctr m }
  where modified = modifyAt (cellptr m) (flip (-) 1) (cells m)

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt _ _ [] = []
modifyAt 0 f (x:xs) = f x : xs
modifyAt n f (x:xs) = x : modifyAt (n - 1) f xs
