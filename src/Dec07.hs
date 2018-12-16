module Dec07 where

-- import Common
-- import Data.List
-- import Data.Maybe
-- import Control.Arrow
-- import Data.Char
-- import Text.Megaparsec.Char as C
-- import qualified Data.Set as Set
-- import qualified Data.Map as Map
-- import Data.Tuple

-- import Data.Graph

-- lineP :: Parser String (Char, Char)
-- lineP = do
--   void $ C.string "Step "
--   c1 <- C.anyChar
--   void $ C.string " must be finished before step "
--   c2 <- C.anyChar
--   void $ takeWhile1P Nothing (const True)
--   pure (c1, c2)

-- testcase =
--   "Step C must be finished before step A can begin.\n\
-- \Step C must be finished before step F can begin.\n\
-- \Step A must be finished before step B can begin.\n\
-- \Step A must be finished before step D can begin.\n\
-- \Step B must be finished before step E can begin.\n\
-- \Step D must be finished before step E can begin.\n\
-- \Step F must be finished before step E can begin."

-- dec07 :: IO ()
-- dec07 = do
--   inp <- readFile "data/dec07.txt"
--   -- let inp = testcase
--   -- P1
--   linewise (pure inp) dec07P1
--   -- P2
--   -- linewise (pure inp) dec07P2

-- data G = G (Set Char) (Map Char [Char])

-- nextElt :: G -> Maybe (Char, G)
-- nextElt (G s ms) =
--   let ms' = filter (fst .> (`Set.notMember` s)) $ Map.toList ms
--   in case ms' of
--     [] -> Nothing
--     (c, nbs):rest -> go c nbs rest
--   where
--     go' = \case
--       [] -> Nothing
--       (c, nbs):rest -> go c nbs rest
--     go c nbs rest =
--       let remdeps = filter (`Set.notMember` s) nbs
--       in case remdeps of
--         [] -> Just (c, G (Set.insert c s) (Map.delete c ms))
--         n:ns -> go' (sort $ (n, fromMaybe [] (Map.lookup n ms)) : rest)

-- mkG :: [(Char, Char)] -> G
-- mkG = G Set.empty . fmap sort . Map.fromListWith (++) . map (second (:[]) . swap)

-- elts cs =
--   let g = mkG cs
--   in unfoldr nextElt g

-- data G' = G' String (Map Char Int) (Map Char [Char])

-- toTiming = map (\c -> (c, 60 + ord c))

-- tStep :: Int -> G' -> Maybe ((), G')
-- tStep nw (G' done wip rem)
--   | Map.size rem == 0 = Nothing
--   | Map.size wip == 0 = tStep nw $ uncurry (G' done) $ shuffle done wip rem
--   | otherwise =
--     let (done', wip') =
--           (map fst .> (++ done)) *** Map.fromList
--           $ partition (snd .> (== 0))
--           $ map (second (subtract 1))
--           $ Map.toList wip
--     in Just ((), uncurry (G' done) (shuffle done wip' rem))
--   where
--     candidates :: String -> Map Char Int -> Map Char [Char] -> [Char]
--     candidates fin wip_ rem_ =
--       -- sort $ Map.keys $ Map.filter (all (\c ->
--       --                                      Map.lookup c rem_ == Nothing ||
--       --                                      c `elem` fin || Map.loo))

--     shuffle :: String -> Map Char Int -> Map Char [Char] -> (Map Char Int, Map Char [Char])
--     shuffle done_ wip_ rems_
--       | Map.size wip_ == nw = (wip_, rems_)
--       | otherwise =
--         let new_wip = take (nw - Map.size wip_) (candidates done_ wip_ rems_)
--         in undefined

-- dec07P1 = map (justParse lineP) .> elts

-- dec07P2 = dec07P1 .> id
