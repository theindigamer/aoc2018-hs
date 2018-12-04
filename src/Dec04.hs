{-# LANGUAGE ViewPatterns #-}
module Dec04 where

import Common
import Data.Semigroup
import Data.List (sortBy, sort, foldl')
import Data.Function (on)

import Debug.Trace

import qualified Data.HashMap.Strict as HM
import qualified Text.Megaparsec.Char as C

data GS = WakeUp | Sleep | Begin Int
  deriving (Eq, Ord, Show)

data Entry =
  Entry { month :: Int, day :: Int, hour :: Int, mins :: Int, info :: GS }
  deriving (Eq, Ord, Show)

gsP :: Parser String GS
gsP =
  C.string "falls asleep" *> pure Sleep
  <|> C.string "wakes up" *> pure WakeUp
  <|> (C.string "Guard #" *> (Begin <$> signedIntP) <* takeWhile1P Nothing (const True))

entryP :: Parser String Entry
entryP =
  Entry
  <$> (C.string "[1518-" *> signedIntP)
  <*> (C.char '-' *> signedIntP)
  <*> (C.space1 *> signedIntP)
  <*> (C.char ':' *> signedIntP)
  <*> (C.string "] " *> gsP)

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

dec04P1 :: IO ()
dec04P1 =
  -- linewise (pure testcase) $
  linewise (readFile "data/dec04.txt") $
      map (justParse entryP)
      .> sort
      .> foldl' go (HM.empty, Nothing, Nothing)
      .> (\(x, y, z) -> x)
      .> HM.toList
      .> map (second (\(HM.toList -> xs) -> (sum $ map snd xs, xs)))
      .> sortBy (flip compare `on` snd)
      .> head
      .> second (head . sortBy (flip compare `on` snd) . snd)
  where
    minsBetween :: Entry -> Entry -> [Int]
    minsBetween sleep wake = [mins sleep .. mins wake - 1]
    f i sleepinfo wakeinfo hm =
      foldl' (\h m -> HM.alter
             (\case {Nothing -> Just (HM.fromList [(m, 1)]);
                     Just g -> Just (HM.alter (\case {Nothing -> Just 1;
                                                      Just i -> Just (i + 1);})
                                      m g)
                    }
             ) i h) hm (minsBetween sleepinfo wakeinfo)
    go (h, prev, cur) e@Entry{info} = case info of
      Begin i -> (h, Nothing, Just i)
      Sleep -> (h, Just e, cur)
      WakeUp -> case (prev, cur) of
        (Just p, Just i) -> (f i p e h, Nothing, Just i)
        _ -> error "Unreachable"

dec04P2 :: IO ()
dec04P2 =
  -- linewise (pure testcase) $
  linewise (readFile "data/dec04.txt") $
      map (justParse entryP)
      .> sort
      .> foldl' go (HM.empty, Nothing, Nothing)
      .> (\(x, y, z) -> x)
      .> HM.toList
      .> sortBy (flip compare `on` snd)
      .> head
  where
    minsBetween :: Entry -> Entry -> [Int]
    minsBetween sleep wake = [mins sleep .. mins wake - 1]
    f i sleepinfo wakeinfo hm =
      foldl' (\h m -> HM.alter
             (\case {Nothing -> Just 1; Just k -> Just (k + 1)})
             (i, m) h) hm (minsBetween sleepinfo wakeinfo)
    go (h, prev, cur) e@Entry{info} = case info of
      Begin i -> (h, Nothing, Just i)
      Sleep -> (h, Just e, cur)
      WakeUp -> case (prev, cur) of
        (Just p, Just i) -> (f i p e h, Nothing, Just i)
        _ -> error "Unreachable"
