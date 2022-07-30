module Brainfuck.Operators where

import Data.Char (chr, ord)
import Data.Map (Map, fromList, (!))
import Data.List.Index (modifyAt)

import System.IO (isEOF)

import Brainfuck.Core
import Brainfuck.Util

opToActionM :: Map Char (BF -> IO BF)
opToActionM = fromList
  [ ('>', return . next)
  , ('<', return . prev)
  , ('+', return . incr)
  , ('-', return . decr)
  , ('[', return . lbeg)
  , (']', return . lend)
  , ('.', cget)
  , (',', cput)
  ]

opToAction :: Char -> BF -> IO BF
opToAction = (opToActionM !)

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

lbeg :: BF -> BF
lbeg m = m { loopind = newstack, progctr = newctr }
  where closeOffset = 1 + findBFLoopEnd (drop (1 + progctr m) $ program m)
        newstack | shouldLoop m = 1 + progctr m : loopind m
                 | otherwise = loopind m
        newctr | shouldLoop m = 1 + progctr m
               | otherwise = 1 + progctr m + closeOffset

lend :: BF -> BF
lend m = m { loopind = newstack, progctr = newprogctr }
  where newstack | shouldLoop m = loopind m
                 | otherwise = tail $ loopind m
        newprogctr | shouldLoop m = head $ loopind m
                   | otherwise = 1 + progctr m

cput :: BF -> IO BF
cput m = do
  modifier <- modifyAt (cellptr m) . const . toEnum . ord <$> tryToGetChar
  return $ m { cells = modifier $ cells m, progctr = 1 +progctr m }
  where tryToGetChar = isEOF >>= \e -> if e then return '\0' else getChar

cget :: BF -> IO BF
cget m = do
  putChar . chr . fromEnum $ cells m !! cellptr m
  return $ m { progctr = 1 + progctr m }
