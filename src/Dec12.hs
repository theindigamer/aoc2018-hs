{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Dec12 (dec12) where

import Common
import Data.Maybe
import Data.Functor

import qualified Text.Megaparsec.Char as C
import qualified Data.HashMap.Strict as HM

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as UV

data Pots = Pots
  { minNegIndex :: !Int
  , pots :: UV.Vector Bool
  }

instance Show Pots where
  show Pots{minNegIndex, pots} =
    let (ixs, ps) = unzip $ zip [minNegIndex ..] (V.toList pots)
    in show ixs <> "\n" <> showView ps

-- e.g. if minNegIndex = -3
-- and length pots = 10, then we should return 6
maxPosIndex :: Pots -> Int
maxPosIndex Pots{minNegIndex, pots} = V.length pots - 1 + minNegIndex

hasPlantAt :: Pots -> Int -> Bool
hasPlantAt Pots{minNegIndex, pots} n =
  let i = n - minNegIndex in
  0 <= i && i < V.length pots && pots V.! i

type View = [Bool]

showView :: View -> String
showView = map showPlant

showPlant :: Bool -> Char
showPlant = \case
  True  -> '#'
  False -> '.'

hasPlantsBetween :: Pots -> (Int, Int) -> View
hasPlantsBetween p (i1, i2) = [i1 .. i2] |> map (p `hasPlantAt`)

type Rules = HashMap View Bool

evolve :: Rules -> Pots -> Pots
evolve rules p@Pots{minNegIndex} =
  [minNegIndex - 2 .. maxPosIndex p + 2]
  |> map (\i -> let v = p `hasPlantsBetween` (i - 2, i + 2) in
                fromMaybe (err v) $ HM.lookup v rules)
  |> UV.fromList
  |> Pots (minNegIndex - 2)
  where
    err v = error ("couldn't find key for " ++ showView v)

plantP :: Parser String Bool
plantP = (C.char '#' $> True) <|> (C.char '.' $> False)

rulesP :: Parser String Rules
rulesP = HM.fromList <$> sepEndBy ruleP C.space
  where
    ruleP = do
      ps <- some plantP
      C.string " => " |> void
      p <- plantP
      pure (ps, p)

potsP :: Parser String Pots
potsP = Pots 0 . UV.fromList <$> some plantP

infoP :: Parser String (Pots, Rules)
infoP = do
  void <| takeWhile1P Nothing (\c -> c /= '#' && c /= '.')
  p <- potsP
  C.space
  r <- rulesP
  void <| optional eof
  pure (p, r)

potIndexSum :: Pots -> Integer
potIndexSum Pots{minNegIndex, pots} =
  V.toList pots |> zip [minNegIndex ..]
  |> filter snd
  |> map fst
  |> sum
  |> fromIntegral

future :: Integer -> Integer -> Rules -> Pots -> Integer
future stop i r p
  | i == stop = potIndexSum p
  | i == 1000 = ((stop - i) * 87) + potIndexSum p
  | otherwise = future stop (i + 1) r (evolve r p)

dec12 :: IO ()
dec12 = do
  inp <- readFile "data/dec12.txt"
  let (p, r) = justParse infoP inp
  print $ future 20 0 r p
  print $ future (5 * 10^10) 0 r p
