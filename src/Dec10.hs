module Dec10 where

import Common
import Data.List
import Data.Maybe

import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec.Char as C

testcase = "position=< 9,  1> velocity=< 0,  2>\n\
\position=< 7,  0> velocity=<-1,  0>\n\
\position=< 3, -2> velocity=<-1,  1>\n\
\position=< 6, 10> velocity=<-2, -1>\n\
\position=< 2, -4> velocity=< 2,  2>\n\
\position=<-6, 10> velocity=< 2, -2>\n\
\position=< 1,  8> velocity=< 1, -1>\n\
\position=< 1,  7> velocity=< 1,  0>\n\
\position=<-3, 11> velocity=< 1, -2>\n\
\position=< 7,  6> velocity=<-1, -1>\n\
\position=<-2,  3> velocity=< 1,  0>\n\
\position=<-4,  3> velocity=< 2,  0>\n\
\position=<10, -3> velocity=<-1,  1>\n\
\position=< 5, 11> velocity=< 1, -2>\n\
\position=< 4,  7> velocity=< 0, -1>\n\
\position=< 8, -2> velocity=< 0,  1>\n\
\position=<15,  0> velocity=<-2,  0>\n\
\position=< 1,  6> velocity=< 1,  0>\n\
\position=< 8,  9> velocity=< 0, -1>\n\
\position=< 3,  3> velocity=<-1,  1>\n\
\position=< 0,  5> velocity=< 0, -1>\n\
\position=<-2,  2> velocity=< 2,  0>\n\
\position=< 5, -2> velocity=< 1,  2>\n\
\position=< 1,  4> velocity=< 2,  1>\n\
\position=<-2,  7> velocity=< 2, -2>\n\
\position=< 3,  6> velocity=<-1, -1>\n\
\position=< 5,  0> velocity=< 1,  0>\n\
\position=<-6,  0> velocity=< 2,  0>\n\
\position=< 5,  9> velocity=< 1, -2>\n\
\position=<14,  7> velocity=<-2,  0>\n\
\position=<-3,  6> velocity=< 2, -1>"

data P = P { px, py, vx, vy :: !Int }
  deriving Show

pprint :: [P] -> T.Text
pprint ps = txt
  where
    pts = HS.fromList $ map (\P{px, py} -> (px, py)) ps
    minx = minimum $ map px ps
    maxx = maximum $ map px ps
    miny = minimum $ map py ps
    maxy = maximum $ map py ps
    txt = T.concat
      [ T.pack ([if (x, y) `HS.member` pts then 'x' else '.' | x <- [minx .. maxx]] ++ "\n")
      | y <- [miny .. maxy]
      ]

pP :: Parser String P
pP = do
  void $ C.string "position=<"
  (px, py) <- twonums
  void $ C.string "> velocity=<"
  (vx, vy) <- twonums
  void $ C.char '>'
  pure $ P { px, py, vx, vy }
  where
    twonums = (,)
      <$> (C.space *> signedIntP)
      <*> (C.char ',' *> C.space *> signedIntP)

step :: Functor f => f P -> f P
step = fmap (\P{px, py, vx, vy} -> P{px = px + vx, py = py + vy, vx, vy})

dec10 :: IO ()
dec10 = do
  inp <- readFile "data/dec10.txt"
  inp |> lines |> map (justParse pP) |> go

nbrs P{px, py} = [ (px + dx, py + dy)
                 | dx <- [ -1 .. 1], dy <- [-1 .. 1]
                 , not (dx == 0 && dy == 0)
                 ]

getMsg :: [P] -> Maybe T.Text
getMsg ps = if isMsg then Just (pprint ps) else Nothing
  where
    isMsg = all (any (`HS.member` pts) . nbrs) ps
    pts = HS.fromList (map (\P{px, py} -> (px, py)) ps)

go ps = do print iter; T.putStrLn t
  where
    (iter, Just t) =
      repeat step
      |> scanl (|>) ps
      |> zipWith (\i x -> (i, getMsg x)) [0 ..]
      |> dropWhile (isNothing . snd)
      |> head
