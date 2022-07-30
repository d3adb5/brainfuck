module Test.Brainfuck where

import Test.QuickCheck
import Test.QuickCheck.Instances.Tuple ((>*<), (>**<))

import Data.Char (chr, ord)
import Data.List (sort)

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
arbitraryBFProgram n = closeBFLoops <$> vectorOf n (elements "+-[]<>,.")

closeBFLoops :: String -> String
closeBFLoops = closeBrackets '[' ']'

closeBrackets :: Char -> Char -> String -> String
closeBrackets op cl s = replicate toOpen op ++ s ++ replicate toClose cl
  where (toOpen, toClose) = missingBrackets op cl s

unmatchedBFLoops :: String -> (Int, Int)
unmatchedBFLoops = missingBrackets '[' ']'

missingBrackets :: Char -> Char -> String -> (Int, Int)
missingBrackets = missingBrackets' 0 0

missingBrackets' :: Int -> Int -> Char -> Char -> String -> (Int, Int)
missingBrackets' to tc _  _  ""     = (to, tc)
missingBrackets' to tc op cl (ch:p)
  | ch == cl && tc == 0 = missingBrackets' (to + 1) tc op cl p
  | ch == cl  = missingBrackets' to (tc - 1) op cl p
  | ch == op  = missingBrackets' to (tc + 1) op cl p
  | otherwise = missingBrackets' to tc       op cl p

nextChar :: Char -> Char
nextChar = chr . (+ 1) . ord
