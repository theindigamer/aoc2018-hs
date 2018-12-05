module Data.OccMap where

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import qualified Data.HashMap.Strict as HM

type OccMap k = HashMap k Int

insert :: (Eq k, Hashable k) => k -> OccMap k -> OccMap k
insert = HM.alter (Just . maybe 1 (+ 1))

fromList :: (Eq k, Hashable k) => [k] -> OccMap k
fromList = HM.fromListWith (+) . (`zip` repeat 1)
