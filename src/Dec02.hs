module Dec02 where

import Common

import qualified Data.HashMap.Strict as HM

import Data.Text (pack, unpack)
import Data.Text.Metrics (levenshtein)

count2three s =
  let els = HM.elems $ HM.fromListWith (+) (zip s [1, 1 ..])
  in (c 3 els, c 2 els)
  where c n xs = Sum $ if n `elem` xs then 1 else 0

dec02P1 :: IO ()
dec02P1 = linewise (readFile "data/dec02.txt") $
  foldMap count2three
  .> uncurry (*)

-- This is really poor but we don't have a lot of data so its okay.
allPairs texts = head
  [ map fst $ filter (uncurry (==)) $ zip (unpack s) (unpack s')
  | s <- texts, s' <- texts, s' /= s, levenshtein s s' <= 1
  ]

dec02P2 :: IO ()
dec02P2 =
  linewise (readFile "data/dec02.txt")
  $ map pack .> allPairs
