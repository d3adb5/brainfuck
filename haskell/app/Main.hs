module Main where

import Data.Default
import System.Environment (getArgs)
import System.IO

import Brainfuck

step :: BF -> IO BF
step m = opToAction (program m !! progctr m) m

run :: BF -> IO ()
run m | progctr m == length (program m) = return ()
      | otherwise = step m >>= run

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  p <- filter (`elem` "+-[]<>,.") <$> (getArgs >>= readFile . head)
  run $ def { program = p }
