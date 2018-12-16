module Dec11 where

import Common
import Data.List
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HM
import Control.Monad.State.Lazy

import qualified Data.Vector as V

data P = P { px, py :: !Int }

snum = 7403

pow s p = powfin
  where
    rid = rackId p
    startpow = rid * py p
    deltapow = s
    tmppow = (startpow + deltapow) * rid
    powfin = ((tmppow `mod` 1000) `div` 100) - 5

startMap2 s = HM.fromList [((x, y),(1, pow s P{px=x, py=y}))
                          | x <- [1 .. 300], y <- [1 .. 300]]

smap = startMap2 snum

ret = maximum $
  [(foldMap (Sum . snd . (smap HM.!))
    [(x', y') | x' <- [x .. x + sz - 1], y' <- [y .. y + sz - 1]]
   , (x, y, sz))
  | x <- [1 .. 300]
  , y <- [1 .. 300]
  , sz <- [1 .. minimum [20, 300 - x + 1, 300 - y + 1]]
  ]

rackId = (+10) . px

dec11 = do
  print ret
