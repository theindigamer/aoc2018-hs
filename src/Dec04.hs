{-# LANGUAGE ViewPatterns #-}
module Dec04 where

import Common
import Data.Semigroup
import Data.Function (on)
import Data.Hashable

import Debug.Trace

import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Text.Megaparsec.Char as C

data GuardState = WakeUp | Sleep | Begin GuardId
  deriving (Eq, Ord, Show)

data Entry =
  Entry { month :: Int, day :: Int, hour :: Int, mins :: Int, info :: GuardState }
  deriving (Eq, Ord, Show)

guardStateP :: Parser String GuardState
guardStateP =
      Sleep  <$ C.string "falls asleep"
  <|> WakeUp <$ C.string "wakes up"
  <|> (  C.string "Guard #"
      *> (Begin <$> signedIntP)
      <* takeWhile1P Nothing (const True)
      )

testcase =
  "[1518-11-01 00:00] Guard #10 begins shift\n\
  \[1518-11-01 00:05] falls asleep\n\
  \[1518-11-01 00:25] wakes up\n\
  \[1518-11-01 00:30] falls asleep\n\
  \[1518-11-01 00:55] wakes up\n\
  \[1518-11-01 23:58] Guard #99 begins shift\n\
  \[1518-11-02 00:40] falls asleep\n\
  \[1518-11-02 00:50] wakes up\n\
  \[1518-11-03 00:00] Guard #10 begins shift\n\
  \[1518-11-03 00:05] falls asleep\n\
  \[1518-11-03 00:25] wakes up"

entryP :: Parser String Entry
entryP =
  Entry
  <$> (C.string "[1518-" *> signedIntP)
  <*> (C.char '-' *> signedIntP)
  <*> (C.space1 *> signedIntP)
  <*> (C.char ':' *> signedIntP)
  <*> (C.string "] " *> guardStateP)

oneOrPlusOne :: (Eq k, Hashable k) => k -> HashMap k Int -> HashMap k Int
oneOrPlusOne = HM.alter (Just . maybe 1 (+ 1))

minsAsleep :: Entry -> Entry -> [Int]
minsAsleep sleep wake = [mins sleep .. mins wake - 1]

type GuardId = Int

go
  :: (GuardId -> HashMap k v -> [Int] -> HashMap k v)
  -> (HashMap k v, Maybe Entry, Maybe Int)
  -> Entry
  -> (HashMap k v, Maybe Entry, Maybe Int)
go f (h, prev, cur) e@Entry{info} = case info of
  Begin i -> (h, Nothing, Just i)
  Sleep -> (h, Just e, cur)
  WakeUp -> case (prev, cur) of
    (Just p, Just i) -> (f i h (minsAsleep p e), Nothing, Just i)
    _ -> error "Unreachable"

dec04P1 :: IO ()
dec04P1 =
  linewise (readFile "data/dec04.txt") $
      map (justParse entryP)
      .> List.sort
      .> List.foldl' (go f) (HM.empty, Nothing, Nothing)
      .> (\(x, y, z) -> x)
      .> HM.toList
      .> map (second (\(HM.toList -> xs) -> (sum $ map snd xs, xs)))
      .> List.maximumBy (compare `on` snd)
      .> second (List.maximumBy (compare `on` snd) . snd)
  where
    f i = foldl' $ \h m -> HM.alter
        (Just . maybe (HM.fromList [(m, 1)]) (oneOrPlusOne m)) i h

dec04P2 :: IO ()
dec04P2 =
  linewise (readFile "data/dec04.txt") $
      map (justParse entryP)
      .> List.sort
      .> List.foldl' (go f) (HM.empty, Nothing, Nothing)
      .> (\(x, y, z) -> x)
      .> HM.toList
      .> List.maximumBy (compare `on` snd)
  where
    f i = foldl' $ \h m -> oneOrPlusOne (i, m) h
