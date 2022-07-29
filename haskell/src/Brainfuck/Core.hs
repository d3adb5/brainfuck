module Brainfuck.Core where

import Data.Default
import Data.Word (Word8)

data BF = BF { program :: String
             , progctr :: Int
             , loopind :: [Int]
             , cellptr :: Int
             , cells :: [Word8]
             } deriving (Show, Eq)

instance Default BF where
  def = BF "" 0 [] 0 [0]
