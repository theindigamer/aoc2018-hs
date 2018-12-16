{-# LANGUAGE RankNTypes #-}

module Dec13 where

import Common
import Control.Monad.ST (runST, ST)
import Data.List (nub, sort)
import Data.Maybe (catMaybes)

import qualified Data.Vector as BV
import qualified Data.Vector.Mutable as BMV
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Fusion.Stream.Monadic as S

data TurnDir = TL | GS | TR
  deriving (Eq, Show)

cycleTurn :: TurnDir -> TurnDir
cycleTurn = \case
  TL -> GS
  GS -> TR
  TR -> TL

data CurDir = U | L | D | R
  deriving (Eq, Show)

adjustTurnDir :: Char -> TurnDir -> TurnDir
adjustTurnDir '+' = cycleTurn
adjustTurnDir _   = id

adjustCurDir :: TurnDir -> CurDir -> CurDir
adjustCurDir GS = id
adjustCurDir TL = \case U -> L; L -> D; D -> R; R -> U
adjustCurDir TR = \case U -> R; L -> U; D -> L; R -> D

getArrowDir :: Char -> Maybe CurDir
getArrowDir = \case
  'v' -> Just D
  '^' -> Just U
  '<' -> Just L
  '>' -> Just R
  _   -> Nothing

data P = P { px, py :: !Int, td :: TurnDir, cd :: CurDir }

instance Show P where
  show P{px, py, td, cd} = "P" <> show (px, py) <> show (td, cd)

instance Eq P where
  p1 == p2 = px p1 == px p2 && py p1 == py p2

instance Ord P where
  compare p1 p2 = comparing px p1 p2 <> comparing py p1 p2

moveCart :: Grid -> P -> P
moveCart g P{px, py, td, cd} = P{px=px', py=py', td=td', cd=cd'}
  where
    px' = case cd of L -> px - 1; R -> px + 1; _ -> px
    py' = case cd of U -> py - 1; D -> py + 1; _ -> py
    c = g `at` (px', py')
    td' = adjustTurnDir c td
    err = error ("encountered non-track char " ++ show c ++ " at " ++ show (px', py'))
    cd' | c == '-' || c == '|'    = cd
        | Just _ <- getArrowDir c = cd
        | c == '+'  = adjustCurDir td cd
        | c == '\\' = case cd of U -> L; L -> U; D -> R; R -> D
        | c == '/'  = case cd of U -> R; R -> U; D -> L; L -> D
        | otherwise = err

sortV :: Ord a => BV.Vector a -> ST s (BMV.MVector s a)
sortV = V.toList .> sort .> BV.fromList .> V.unsafeThaw

sortMV :: Ord a => BMV.MVector s a -> ST s (BMV.MVector s a)
sortMV = V.unsafeFreeze >=> sortV

type Carts s = BMV.MVector s P

type Grid = BV.Vector (UV.Vector Char)

mkGrid :: String -> Grid
mkGrid = lines .> map V.fromList .> V.fromList

at :: Grid -> (Int, Int) -> Char
at g (x, y) = (g V.! y) V.! x

dec13p1 :: Grid -> Carts s -> ST s P
dec13p1 g cs = do
  (p :|_, _) <- tickTillCollisions g cs
  pure p

dec13p2 :: Grid -> Carts s -> ST s P
dec13p2 g cs = do
  (_, cs') <- tickTillCollisions g cs
  if MV.length cs' == 1 then MV.read cs' 0
  else dec13p2 g cs'

tickTillCollisions :: Grid -> Carts s -> ST s (NonEmpty P, Carts s)
tickTillCollisions g cs = do
  surviving <- BMV.replicate (MV.length cs) True
  colliding_dupes <- forM [0 .. MV.length cs - 1] <| \i -> do
    surv_i <- MV.read surviving i
    if surv_i then do
      stepCart cs i
      checkCrash surviving cs i
    else pure []
  let colliding = nub (concat colliding_dupes)
  let delete = S.indexed .> S.filterM (fst .> MV.read surviving) .> S.map snd
  cs |> case colliding of
    []     -> sortMV >=> tickTillCollisions g
    (x:xs) -> MV.transform delete >=> sortMV .> fmap (x:|xs,)
  where
    stepCart cx = MV.modify cx (moveCart g)
    checkCrash surviving cx i = do
        c <- MV.read cx i
        rets <- forM [0 .. MV.length cx - 1] $ \j -> do
          surv_j <- MV.read surviving j
          if not surv_j || j == i then pure Nothing
          else do
            c' <- MV.read cx j
            if c == c' then do
              MV.write surviving i False
              MV.write surviving j False
              pure (Just c)
            else pure Nothing
        pure $ catMaybes rets

findCarts :: Grid -> BV.Vector P
findCarts =
  V.imap (\i -> V.convert .> V.imapMaybe
           (\j -> getArrowDir .> fmap (\d -> P{px=j, py=i, td=TL, cd=d})))
  .> join

dec13 = do
  inp <- readFile "data/dec13.txt"
  print $ solveP dec13p1 inp
  print $ solveP dec13p2 inp
  where
    solveP :: (forall s. Grid -> Carts s -> ST s P) -> String -> P
    solveP p i = runST $ do
        let g = mkGrid i
        cs <- sortV <| findCarts g
        p g cs
