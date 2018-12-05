module Dec05 where

import Common
import Data.Char
import Data.Maybe
import Data.Function
import Data.List

dec05P1 :: IO ()
dec05P1 = readFile "data/dec05.txt" >>= reactFull .> print

reaction :: String -> String
reaction = go []
  where
    go xs [] = reverse xs
    go xs (c:cs) = case cs of
      [] -> go (c:xs) []
      c':cs' -> if (isLower c && c' == toUpper c) || (isUpper c && c' == toLower c)
            then (case xs of x:xs' -> go xs' (x:cs'); [] -> go [] cs')
            else go (c:xs) cs

reactFull s = let s' = reaction s in if s' == s then length s' else reactFull s'

dec05P2 :: IO ()
dec05P2 = readFile "data/dec05.txt" >>= go .> print
  where
    go s =
      sort [ (n, c)
           | c <- ['a' .. 'z']
           , let n = reactFull $ filter (\c' -> c' /= c && c' /= toUpper c) s
           ]
