{-# LANGUAGE TypeApplications #-}

module Dec01 where

import Data.Monoid (Sum(..))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

getInts :: IO [Int]
getInts = map readInt . lines <$> readFile "data/dec01.txt"
  where
    readInt ('+':cs) = read @Int cs
    readInt cs = read @Int cs

dec01P1 :: IO ()
dec01P1 = print . sum =<< getInts

dec01P2 :: IO ()
dec01P2 = do
  ints <- getInts
  let go s (x:xs) = if IntSet.member x s then x else go (IntSet.insert x s) xs
  -- NOTE: scanl1 is the good guy, scanr1 doesn't work with infinite lists
  print . go mempty . map getSum . scanl1 (<>) . map Sum . concat $ repeat ints
