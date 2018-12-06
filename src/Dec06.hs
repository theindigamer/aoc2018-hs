module Dec06 where

import Common
import Data.Char
import Data.Maybe
import Data.Function
import Data.List
import Data.Bifunctor

data Coords = C { x :: Int, y :: Int }
  deriving (Eq, Ord, Show)

isOnBoundary :: Coords -> [Coords] -> Bool
isOnBoundary C{x, y} cs =
  null [() | C x1 y1 <- cs, x1 < x && y1 < y]
  || null [() | C x1 y1 <- cs, x1 > x && y1 < y]
  || null [() | C x1 y1 <- cs, x1 < x && y1 > y]
  || null [() | C x1 y1 <- cs, x1 > x && y1 > y]

segregate :: [Coords] -> ([Coords], [Coords])
segregate cs =
  (bimap (map snd) (map snd)) $ partition fst $ zip (map (`isOnBoundary` cs) cs) cs

getPoint :: String -> Coords
getPoint s =
  let x = read $ (takeWhile (/= ',')) s
      y = read $ (dropWhile (== ' ') . dropWhile (/= ' ')) s
  in C{x, y}

dist (C x1 y1) (C x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- f :: Coords -> [Coords] -> Maybe (Coords, Int)
f b c cs = case sortOn snd $ map (\c' -> (c', dist c c')) cs of
  (c1, d1):(c2, d2):_ | (b || d1 < d2) -> Just (c1, d1)
  _ -> Nothing

cmpx = comparing x
cmpy = comparing y

testcase =
  "1, 1\n\
  \1, 6\n\
  \8, 3\n\
  \3, 4\n\
  \5, 5\n\
  \8, 9"

dec06P1 :: IO ()
-- dec06P1 = linewise (pure testcase) $
dec06P1 = linewise (readFile "data/dec06.txt") $
  map getPoint .> segregate .>
  (\(bounds, ins) ->
     let xmin = x $ minimumBy cmpx bounds
         ymin = x $ minimumBy cmpx bounds
         xmax = y $ maximumBy cmpy bounds
         ymax = y $ maximumBy cmpy bounds
     -- in ins
     in (length &&& id)
       $ maximumBy (comparing length)
       $ group $ sort
        $ [c | x <- [xmin .. xmax], y <- [ymin .. ymax]
            , let (r, c) = case (f True C{x, y} bounds, f False C{x,y} ins) of
                    (Just bdist, Just indist) -> (snd indist < snd bdist, fst indist)
                    (Just bdist, Nothing) -> (False, C 0 0)
                    _ -> (False, C 0 0)
            , r
          ]
  )

alternate (x:xs) (y:ys) = x:y:alternate xs ys

deltas d c@C{x, y} =
  if d == 0 then [c]
  else concatMap (\dx -> let dy = d - dx in
                [C (x + dx) (y + dy)
                , C (x - dx) (y - dy)
                , C (x + dx) (y - dy)
                , C (x - dx) (y + dy)
                ]) [0 .. d]

dec06P2 = linewise (readFile "data/dec06.txt") $
  map getPoint .>
  (\cs ->
     let np = length cs
         xs = sort $ map x cs
         xmin = head xs
         xmax = last xs
         ys = sort $ map y cs
         ymin = head ys
         ymax = last ys
         xmid = (xmin + xmax) `div` 2
         ymid = (ymin + ymax) `div` 2
         sumDist c = sum $ map (dist c) cs
         d = 10000
     in
     sum $ map (length . nub)
     $ takeWhile (not . null)
     $ map (\d' -> filter ((< d) . sumDist) $ deltas d' (C xmid ymid))
     $ [0 ..]
  )
