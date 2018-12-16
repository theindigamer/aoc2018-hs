{-# OPTIONS_GHC -funbox-strict-fields #-}

module Dec14 (dec14) where

import Prelude hiding (length)
import Common hiding (length)
import Data.Char
import Control.Monad.ST
import Control.Monad.Primitive

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MUV
import qualified Data.Vector.Fusion.Stream.Monadic as MS

type Index = Int
type Score = Int

data CapVector s a = CapVector !Int !(MUV.MVector s a)

length :: CapVector s a -> Int
length (CapVector l _) = l

vec :: (MV.MVector MUV.MVector a) => CapVector s a -> MUV.MVector s a
vec (CapVector l v) = MV.take l v

push
  :: (PrimMonad m, s ~ PrimState m, MV.MVector MUV.MVector a)
  => a -> CapVector s a -> m (CapVector s a)
push x (CapVector l v) = do
  v' <- if MV.length v == l then MV.grow v l else pure v
  MV.write v' l x
  pure (CapVector (l + 1) v')

unsafeRead :: CapVector s Score -> Index -> ST s Score
unsafeRead (CapVector _ v) = MV.unsafeRead v

step :: CapVector s Score -> Index -> Index -> ST s (CapVector s Score, Int, Int)
step v i1 i2 = do
  s1 <- v `unsafeRead` i1
  s2 <- v `unsafeRead` i2
  let d2 = (s1 + s2) `mod` 10
  v' <- v |> (if s1 + s2 > 9 then push 1 else pure) >>= push d2
  let i1' = (i1 + 1 + s1) `mod` length v'
  let i2' = (i2 + 1 + s2) `mod` length v'
  pure (v', i1', i2')
{-# INLINE step #-}

runExpt :: Int -> UV.Vector Int
runExpt start = V.take 10 (V.drop start v)
  where
    end = start + 10
    go :: CapVector s Score -> Int -> Int -> ST s (UV.Vector Score)
    go z i1 i2 =
      if length z >= end then V.unsafeFreeze (vec z)
      else do
        (z', i1', i2') <- step z i1 i2
        go z' i1' i2'
    v = runST $ do
      z <- CapVector 2 <$> V.unsafeThaw (UV.fromList [3, 7])
      let i1 = 0
      let i2 = 1
      go z i1 i2

eqMVec
  :: (Eq a, PrimMonad m, PrimState m ~ s, MV.MVector MUV.MVector a)
  => MUV.MVector s a -> MUV.MVector s a -> m Bool
eqMVec v1 v2 =
  if MV.length v1 == MV.length v2 then
    if MV.length v1 == 0 then pure True
    else MS.eqBy (==) (MV.mstream v1) (MV.mstream v2)
  else pure False
{-# INLINE eqMVec #-}

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb mx my = do b <- mb; if b then mx else my
{-# INLINE ifM #-}

runExpt2 :: Int -> Int
runExpt2 start = runST $ do
  cv <- CapVector 2 <$> V.unsafeThaw (UV.fromList [3, 7])
  matchVec <- V.thaw startSplit
  go matchVec cv 0 1
  where
    startSplit = UV.fromList $ map digitToInt $ show start
    matchLen = V.length startSplit
    go :: MUV.MVector s Score -> CapVector s Score -> Int -> Int -> ST s Int
    go matchVec z i1 i2 = do
      let l2 = length z - matchLen
      let zv = vec z
      ifM (matchVec `eqMVec` MV.drop l2 zv)
          (pure l2) $
          ifM (matchVec `eqMVec` MV.take matchLen (MV.drop (l2 - 1) zv))
              (pure (l2 - 1)) $ do
                (z', i1', i2') <- step z i1 i2
                go matchVec z' i1' i2'

dec14 :: IO ()
dec14 = do
  let inp = 293801
  print (runExpt inp)
  print (runExpt2 inp)
