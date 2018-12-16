module Dec09 where

import Common hiding (State)
import Data.List
import Data.Maybe
import Control.Monad.State.Strict

import Data.IntMap (IntMap)
import qualified Data.CircularList as CL
import qualified Data.IntMap as IntMap

np = 423
lm = 71944

data S = S
  { numP :: !Int
  , curP :: !Int
  , marble :: !Int
  , scores :: IntMap Int
  , circ :: CL.CList Int
  } deriving Show

step :: State S ()
step = do
  i <- gets marble
  if i `mod` 23 == 0 && i /= 0 then do
    s <- get
    let cl' = CL.rotN (-7) (circ s)
        elt = fromJust $ CL.focus cl'
        scores' = IntMap.adjust (+ (i + elt)) (curP s) (scores s)
    put s{circ = CL.removeR cl', scores = scores'}
  else
    modify (\s -> s{circ = circ s |> CL.rotN 2 .> CL.insertR i})
  modify (\s -> s{curP = (curP s + 1) `mod` numP s, marble = i + 1})

hs np lm =
  let is = S { numP = np, curP = 0, marble = 1
             , scores = IntMap.fromList $ map (,0) [0 .. np - 1]
             , circ = CL.singleton 0
             }
      (rest, fs) = runState (replicateM_ lm step) is
  in (rest, maximumBy (comparing snd) $ IntMap.toList $ scores fs)

dec09 :: IO ()
dec09 = do
  inp <- readFile "data/dec09.txt"
  print $ hs 423 71944
  print $ hs 423 (71944 * 100)
