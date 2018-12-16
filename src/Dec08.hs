module Dec08 where

import Common

import qualified Data.Vector as V

testcase = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

ds :: String -> [Int]
ds = map read . words

data T' m = T Int [T' m] Int [m]
  deriving (Show, Functor, Foldable, Traversable)

type T = T' Int

getTreeN :: Int -> [Int] -> ([T], [Int])
getTreeN = go []
  where
    go t 0 is = (reverse t, is)
    go ts n is =
      let (t, rest) = getTree is
      in go (t : ts) (n - 1) rest

getTree :: [Int] -> (T, [Int])
getTree (nc:nmeta:xs) =
  let (kids, rest) = getTreeN nc xs
      (meta, rest') = splitAt nmeta rest
  in (T nc kids nmeta meta, rest')

metaSum :: T -> Int
metaSum = getSum . foldMap Sum

val :: T -> Int
val (T nc kids nm metas) =
  if nc == 0 then sum metas
  else sum [(\case Just x -> val x; Nothing -> 0)
            $ V.fromList kids V.!? (m - 1) | m <- metas, m /= 0]

dec08 :: IO ()
dec08 = do
  inp <- readFile "data/dec08.txt"
  -- let inp = testcase
  -- print $ first metaSum $ getTree $ dec08p1 inp
  print $ first val $ (\(t, xs) -> (t, t)) $ getTree $ dec08p1 inp
  -- linewise (pure inp) dec08p2

dec08p1 = ds

dec08p2 = id
