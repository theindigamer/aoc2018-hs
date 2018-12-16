module Dec06 where

import Common
import Data.List
import Control.Arrow

data Coord = C { x :: Int, y :: Int }
  deriving (Eq, Ord, Show)

isOnBoundary :: Coord -> [Coord] -> Bool
isOnBoundary C{x, y} cs =
     null [() | C x1 y1 <- cs, x1 < x && y1 < y]
  || null [() | C x1 y1 <- cs, x1 > x && y1 < y]
  || null [() | C x1 y1 <- cs, x1 < x && y1 > y]
  || null [() | C x1 y1 <- cs, x1 > x && y1 > y]

segregate :: [Coord] -> ([Coord], [Coord])
segregate cs =
   (map snd *** map snd) $ partition fst $ zip (map (`isOnBoundary` cs) cs) cs

getPoint :: String -> Coord
getPoint s =
  let x = read $ takeWhile (/= ',') s
      y = read $ (dropWhile (== ' ') . dropWhile (/= ' ')) s
  in C{x, y}

dist (C x1 y1) (C x2 y2) = abs (x1 - x2) + abs (y1 - y2)

getClosest :: Bool -> Coord -> [Coord] -> Maybe (Coord, Int)
getClosest uniq c cs =
  case sortOn snd $ map (\c' -> (c', dist c c')) cs of
    (c1, d1):(c2, d2):_ | not uniq || d1 < d2 -> Just (c1, d1)
    _ -> Nothing

testcase =
  "1, 1\n\
  \1, 6\n\
  \8, 3\n\
  \3, 4\n\
  \5, 5\n\
  \8, 9"

dec06P1 :: IO ()
dec06P1 = linewise (readFile "data/dec06.txt") $
  map getPoint .> segregate .>
  (\(bounds, ins) ->
     let xs = sort $ map x bounds
         xmin = head xs
         xmax = last xs
         ys = sort $ map y bounds
         ymin = head ys
         ymax = last ys
     in [ c | x <- [xmin .. xmax], y <- [ymin .. ymax]
        , let (closeToInner, c) =
                case (getClosest False C{x, y} bounds, getClosest True C{x,y} ins) of
                  (Just bdist, Just indist) -> (snd indist < snd bdist, fst indist)
                  (Just bdist, Nothing) -> (False, C 0 0)
                  _ -> error "Unreachable" -- Ah, this is not nice :(
        , closeToInner
        ]
        |> sort
        |> group
        |> maximumBy (comparing length)
        |> length &&& id
  )

deltas :: Coord -> Int -> [Coord]
deltas c@C{x, y} d =
  if d == 0 then [c]
  else concatMap
    (\dx -> let dy = d - dx in
        [ C (x + dx) (y + dy)
        , C (x - dx) (y - dy)
        , C (x + dx) (y - dy)
        , C (x - dx) (y + dy)
        ]
    )
    [0 .. d]

dec06P2 :: IO ()
dec06P2 = linewise (readFile "data/dec06.txt") $
  map getPoint .>
  (\cs ->
     let np = length cs
         xs = sort $ map x cs
         ys = sort $ map y cs
         xmid = (head xs + last xs) `div` 2
         ymid = (head ys + last ys) `div` 2
         sumDist c = sum $ map (dist c) cs
         d = 10000
     in [0 ..]
        |> map (deltas (C xmid ymid) .> filter (sumDist .> (< d)))
        |> takeWhile (not . null)
        |> map (nub .> length) -- nub to remove duplicates for delta = 0
        |> sum
  )
