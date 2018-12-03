module Dec03 where

import Control.Arrow
import Common
import Data.Semigroup

import qualified Data.HashMap.Strict as HM

import qualified Text.Megaparsec.Char as C

data Claim = Claim { id_ :: Int, dx :: Int, dy :: Int, w :: Int, h :: Int }
  deriving Show

claimP :: Parser String Claim
claimP = do
  void (C.char '#')
  id_ <- lexeme signedIntP
  void (lexeme (C.char '@'))
  dx <- signedIntP
  void (C.char ',')
  dy <- signedIntP
  void (lexeme (C.char ':'))
  w <- signedIntP
  void (C.char 'x')
  h <- signedIntP
  pure $ Claim {id_, dx, dy, w, h}

testcase = "#1 @ 1,3: 4x4\n\
           \#2 @ 3,1: 4x4\n\
           \#3 @ 5,5: 2x2"

dec03P1 :: IO ()
dec03P1 = linewise (readFile "data/dec03.txt") $
  \s ->
    map (justParse claimP) s
    |> concatMap
        (\Claim{dx, dy, w, h} -> [(x, y)| x <- [dx .. dx + w - 1], y <- [dy .. dy + h - 1]])
    |> HM.fromListWith (+) . (`zip` repeat (1 :: Int))
    |> HM.elems
    |> filter (>= 2) |> length

dec03P2 :: IO ()
dec03P2 = linewise (readFile "data/dec03.txt") $
  \s ->
    map (justParse claimP) s
    |> concatMap
        (\Claim{id_, dx, dy, w, h} ->
           [(id_, x, y)| x <- [dx .. dx + w - 1], y <- [dy .. dy + h - 1]])
    |> map (\(id_, x, y) -> ((x, y), (Sum 1, [id_])))
    |> HM.fromListWith (<>)
    |> HM.elems
    |> concatMap (\(s, ixs) -> [(ix, Max (getSum s)) | ix <- ixs])
    |> HM.fromListWith (<>)
    |> HM.filter (Max 1 ==)
    |> HM.toList
