{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Dec16 where

import Common hiding (match)

import Control.Monad.ST
import Control.Exception (assert)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)

import Data.Bits ((.|.), (.&.))

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Vector as BV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MUV

data Op
  = Addr | Addi
  | Mulr | Muli
  | Banr | Bani
  | Borr | Bori
  | Setr | Seti
  | Gtir | Gtri | Gtrr
  | Eqir | Eqri | Eqrr
  deriving (Eq, Ord, Enum, Show)

cond :: Bool -> a -> Maybe a
cond True a = Just a
cond False _ = Nothing

type Reg s = MUV.MVector s Int

data Instr o = Instr { op :: o, in1 :: !Int, in2 :: !Int, out :: !Int }
  deriving Show

opAction :: Instr Op -> Reg s -> ST s ()
opAction Instr{op, in1, in2, out} = case op of
  Addr -> regReg (+)
  Addi -> regVal (+)
  Mulr -> regReg (*)
  Muli -> regVal (*)
  Banr -> regReg (.&.)
  Bani -> regVal (.&.)
  Borr -> regReg (.|.)
  Bori -> regVal (.|.)
  Setr -> \v -> do x1 <- MV.read v in1; MV.write v out x1
  Seti -> \v -> MV.write v out in1
  Gtir -> valReg (>)
  Gtri -> regVal (>)
  Gtrr -> regReg (>)
  Eqir -> valReg (==)
  Eqri -> regVal (==)
  Eqrr -> regReg (==)
  where
    (>) a b = fromEnum (a Prelude.> b)
    (==) a b = fromEnum (a Prelude.== b)
    valReg f v = do x2 <- MV.read v in2; MV.write v out (f in1 x2)
    regReg f v = do x1 <- MV.read v in1; x2 <- MV.read v in2; MV.write v out (f x1 x2)
    regVal f v = do x1 <- MV.read v in1; MV.write v out (f x1 in2)

data Item = Item { before, after :: UV.Vector Int, instr :: Instr Int}
  deriving Show

vecP :: Parser String (UV.Vector Int)
vecP =
  UV.fromList <$>
  between (C.char '[') (C.char ']') (L.decimal `sepBy` (C.char ',' <* C.space))

type Possible = IntMap Op

match :: Item -> [Op]
match Item{before, after, instr = Instr{in1, in2, out}}=
  [Addr .. Eqrr]
  |> mapMaybe (\o ->
                 let instr = Instr{op=o, in1, in2, out}
                     res = runST $ do
                       v <- V.thaw before
                       opAction instr v
                       V.unsafeFreeze v
                 in cond (after == res) o
              )

matchIM :: Item -> IntMap (Set Op)
matchIM i@Item{instr = Instr{op = opnum}} =
  match i |> Set.fromList |> \xs -> IM.fromList [(opnum, xs)]

data File = File {items :: BV.Vector Item, program :: BV.Vector (Instr Int)}
  deriving Show

instrP :: Parser String (Instr Int)
instrP = do
  op  <- L.decimal <* C.space
  in1 <- L.decimal <* C.space
  in2 <- L.decimal <* C.space
  out <- L.decimal <* C.space
  pure Instr{op, in1, in2, out}

fileP :: Parser String File
fileP = File
  <$> (V.fromList <$> itemP `sepEndBy` C.space)
  <*> (V.fromList <$> some instrP <* (C.space *> eof))

itemP :: Parser String Item
itemP = do
  before <- C.string "Before:" *> C.space *> vecP <* C.space
  instr <- instrP
  after <- C.string "After:" *> C.space *> vecP <* C.space
  pure <| Item before after instr

simulate :: Foldable t => BV.Vector Op -> t (Instr Int) -> UV.Vector Int
simulate opv raw_instrs = runST $ do
  v <- V.unsafeThaw (UV.replicate 4 0)
  forM_ raw_instrs $ \Instr{op = opnum, in1, in2, out} -> do
    let instr = Instr{op = opv V.! opnum, in1, in2, out}
    opAction instr v
  V.unsafeFreeze v

dec16 :: IO ()
dec16 = do
  inp <- readFile "data/dec16.txt"
  let x = justParse fileP inp
  items x |> V.filter (match .> length .> (>= 3)) .> length .> print
  let opv = items x
          |> V.map matchIM .> V.toList
          .> IM.unionsWith Set.intersection
          .> deduce Map.empty
  simulate opv (program x) |> print
  where
    deduce sofar ims =
      if IM.null ims
      then Map.toList sofar |> sortOn snd .> map fst .> BV.fromList
      else let oplist =
                 [(Set.findMin ys, x) | (x, ys) <- IM.toList ims, Set.size ys == 1]
               go = foldl' (\imap (opname, _) ->
                             IM.filter (not . Set.null)
                             $ IM.map (Set.delete opname) imap)
               sofar' = sofar <> Map.fromList oplist
           in assert (not (null oplist)) <| deduce sofar' (go ims oplist)
