{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Dec15 (dec15) where

import Common

import Control.Exception (assert)
import Data.List (sortOn)
import Data.IORef (writeIORef, readIORef, newIORef, IORef)
import Data.Maybe (fromMaybe, fromJust, isJust, catMaybes, mapMaybe)
import Data.Tuple (swap)
import System.IO.Unsafe (unsafePerformIO)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Data.Graph.Inductive.Graph (Node)
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.SP as GQ

import qualified Text.Megaparsec.Char as C

-- General stuff

for :: [a] -> (a -> b) -> [b]
for = flip map

cond :: Bool -> a -> Maybe a
cond True a  = Just a
cond False _ = Nothing

-- Units and enemy checks

data UnitType = Goblin | Elf
  deriving (Show, Eq)

enemy :: UnitType -> UnitType
enemy Goblin = Elf
enemy Elf = Goblin

data Tile a = W | C | G a | E a
  deriving (Show, Eq)

-- Cave and position handling

type Cave a = Seq (Seq (Tile a))

instance {-# OVERLAPPING #-} Show (Cave a) where
  show = fmap showrow .> toList .> unlines
    where
      showrow = toList .> map showU
      showU = \case
        G _ -> 'G'
        E _ -> 'E'
        C -> '.'
        W -> '#'

caveP :: Parser String (Cave ())
caveP = Seq.fromList <$> some (Seq.fromList <$> (some tileP <* C.space))
  where
    tileP =   W    <$ C.char '#'
          <|> C    <$ C.char '.'
          <|> G () <$ C.char 'G'
          <|> E () <$ C.char 'E'

data P = P{py, px :: !Int}
  deriving (Show, Eq, Ord)

at :: Cave a -> P -> Tile a
at rows P{px, py} = (rows `Seq.index` py) `Seq.index` px

reset :: Tile a -> P -> Cave a -> Cave a
reset elt P{px, py} = Seq.adjust (Seq.adjust (const elt) px) py

unsafeCleanupCorpse :: P -> Cave a -> Cave a
unsafeCleanupCorpse = reset C

neighbors :: P -> [P]
neighbors P{px, py} =
  [ P{px = px, py = py - 1}
  , P{px = px, py = py + 1}
  , P{px = px + 1, py = py}
  , P{px = px - 1, py = py}
  ]

loop :: ([b] -> [a]) -> (Int -> Int -> b) -> Cave i -> [a]
loop filtermap f rows =
  concat . for [1 .. Seq.length rows - 2] $ \y ->
    filtermap $ for [1 .. Seq.length (rows `Seq.index` 0) - 2] $ \x -> f x y

-- Graph construction and querying

type CaveGraph = Gr P Word

-- The graph is known to be small, so 128 is sufficient.
pToN :: P -> Node
pToN P{px, py} = py * 128 + px

mkGraph :: Eq i => Cave i -> CaveGraph
mkGraph c = loop id go c |> mconcat .> uncurry G.mkGraph
  where
    isEdge u1 u2 = u2 == C && u1 /= W
    go x y = ([(pToN p, p)], edges)
      where
        p = P{px=x, py=y}
        edges = neighbors p
                |> mapMaybe (\p' -> cond (isEdge (c `at` p) (c `at` p'))
                                         (pToN p, pToN p', 1))

-- Units, armies and combatants

type HitPoints = Word

data Unit = Unit { pos :: P, hitPoints :: HitPoints }
  deriving (Show, Eq, Ord)

type Id = Int
type IdMap = IntMap
type Army = IdMap Unit

instance {-# OVERLAPPING #-} Show Army where
  show = IM.toList .> show

data Combatants = Combatants {goblins :: Army, elves :: Army, startElfId :: !Int}

instance Show Combatants where
  show Combatants{elves, goblins} =
    "Elves " ++ show elves ++ "\n" ++ "Goblins " ++ show goblins

isGoblin :: Id -> Combatants -> Bool
isGoblin i ges = i < startElfId ges

modifyArmy :: UnitType -> (Army -> Army) -> Combatants -> Combatants
modifyArmy Goblin f ges = ges{goblins = f (goblins ges)}
modifyArmy Elf f ges = ges{elves = f (elves ges)}

getEnemies, getComrades :: UnitType -> Combatants -> Army
getEnemies Goblin ges = elves ges
getEnemies Elf ges = goblins ges
getComrades = getEnemies . enemy -- The enemy of my enemy is my friend.

-- State initialization

initUnit :: P -> Unit
initUnit pos = Unit {pos, hitPoints = 200}

getArmy :: Id -> Tile () -> Cave () -> (Army, Map P Id)
getArmy start elt c =
  loop catMaybes go c
  |> zip [start ..]
  |> IM.fromList &&& (map (swap .> first pos) .> Map.fromList)
  where
    go x y = cond (c `at` P{py=y, px=x} == elt)
                  (initUnit P{py=y, px=x})

updateCave :: Cave () -> (Combatants, Cave Id)
updateCave c = (ges, update c)
  where
    (a1, gs) = getArmy 0 (G ()) c
    id_diff = IM.size a1
    (a2, es) = getArmy id_diff (E ()) c
    ges = Combatants {goblins = a1, elves = a2, startElfId = id_diff}
    update = Seq.mapWithIndex $
      \y -> Seq.mapWithIndex $
            \x elt -> case elt of
              G _ -> G (gs Map.! P{px=x, py=y})
              E _ -> E (es Map.! P{px=x, py=y})
              W -> W
              C -> C

-- Play

data Distance = Finite !Word | Infinite
  deriving (Eq, Ord)

instance Show Distance where
  show = \case
    Finite i -> show i
    Infinite -> "Inf"

decideStep :: CaveGraph -> P -> Set P -> Maybe (Distance, P)
decideStep gr currentP =
  Set.map (distance currentP &&& id)
  .> Set.lookupMin
  .> fmap (uncurry stepP)
  where
    distance p1 p2 = case GQ.spLength (pToN p1) (pToN p2) gr of
      Just i  -> Finite i
      Nothing -> Infinite
    outOfBounds = P{px= -1, py= -1}
    stepP distToTargetP targetP = (distToTargetP,) <|
      if distToTargetP == Infinite then outOfBounds
      else snd $ minimum [ (dist, nbr)
                         | nbr <- neighbors currentP,
                           G.hasEdge gr (pToN currentP, pToN nbr),
                           let dist = distance nbr targetP
                         ]

turn :: Id -> Combatants -> Cave Id -> (Combatants, Cave Id)
turn i ges@Combatants{goblins, elves} cave
  | IM.null elves || IM.null goblins
    || (i `IM.notMember` goblins && i `IM.notMember` elves) = (ges, cave)
  -- still alive and enemies are present
  | otherwise = case best_path_info of
    Just (Finite 0, curPos)    -> attack (pickEnemy curPos) ges cave
    Just (Finite 1, attackPos) -> uncurry (attack (pickEnemy attackPos))
                                  (applyMove attackPos i ges cave)
    Just (Finite _, stepPos)   -> applyMove stepPos i ges cave
    Just (Infinite, _)         -> (ges, cave)
    Nothing                    -> (ges, cave)
  where
    best_path_info
      | i_pos `Set.member` enemy_adjs = Just (Finite 0, i_pos)
      | otherwise = decideStep (mkGraph cave) i_pos enemy_adjs
    ut = if isGoblin i ges then Goblin else Elf
    i_pos = pos (getComrades ut ges IM.! i)
    enemy_adjs = getEnemies ut ges
      |> IM.elems .> concatMap (pos .> neighbors) .> Set.fromList
    enemyAt po = case cave `at` po of
      G j | ut == Elf    -> Just (hitPoints $ goblins IM.! j, j)
      E j | ut == Goblin -> Just (hitPoints $ elves   IM.! j, j)
      _ -> Nothing
    pickEnemy new_i_pos = snd $ minimum
      [ ((health, nbr), enemyId)
      | nbr <- neighbors new_i_pos, let enem = enemyAt nbr
      , isJust enem, let (health, enemyId) = fromJust enem
      ]

-- Global mutable variables, yay!
elfAttackPower :: IORef Word
elfAttackPower = unsafePerformIO $ newIORef 3
{-# NOINLINE elfAttackPower #-}

attackPower :: UnitType -> Word
attackPower = \case
  Goblin -> 3
  Elf -> unsafePerformIO (readIORef elfAttackPower)
{-# NOINLINE attackPower #-}

data KillCam loc a = Kill loc a | NoKill a
  deriving Functor

-- | TODO: Take care of health here.
attack :: Id -> Combatants -> Cave Id -> (Combatants, Cave Id)
attack i_underAttack ges c = case go comrades of
  Kill loc army -> (setArmy army ges, unsafeCleanupCorpse loc c)
  NoKill army -> (setArmy army ges, c)
  where
    go = IM.alterF (fromMaybe err .> landHit) i_underAttack
    comrades = getComrades ut ges
    setArmy ar = modifyArmy ut (const ar)
    damage = attackPower (enemy ut)
    landHit un@Unit{hitPoints} =
      if hitPoints < damage
      then Kill (pos <| comrades IM.! i_underAttack) Nothing
      else NoKill (Just un{hitPoints = hitPoints - damage})
    err = error ("Expected " ++ show ut ++ " in map")
    ut = if isGoblin i_underAttack ges then Goblin else Elf

applyMove :: P -> Id -> Combatants -> Cave Id -> (Combatants, Cave Id)
applyMove p' i ges c = (comb', c')
  where
    ut = if isGoblin i ges then Goblin else Elf
    comr = getComrades ut ges
    Unit{pos, hitPoints} = comr IM.! i
    comb' = modifyArmy ut (IM.insert i Unit{pos=p', hitPoints}) ges
    c' = c |> reset C pos |> reset (if ut == Goblin then G i else E i) p'

oneRound :: [Id] -> Combatants -> Cave Id -> (Bool, Combatants, Cave Id)
oneRound = go
  where
    go [] combs cave = (True, combs, cave)
    go (i:ids) combs@Combatants{elves, goblins} cave
      | IM.null elves || IM.null goblins = (False, combs, cave)
      | otherwise = uncurry (go ids) (turn i combs cave)

getPlayOrder :: Combatants -> [Id]
getPlayOrder Combatants{goblins, elves} =
  map fst $ sortOn snd (IM.toList elves <> IM.toList goblins)

gameOver :: Combatants -> Bool
gameOver ges = IM.null (goblins ges) || IM.null (elves ges)

results :: (Show a1, Show a2, Num a1) => a1 -> Combatants -> a2 -> IO (a1, HitPoints)
results i ges cv = do
  print ("Final round number " ++ show i)
  print "Final cave state"
  print cv
  print "Goblins"
  hsum1 <- printArmy (goblins ges)
  print "Elves"
  hsum2 <- printArmy (elves ges)
  let ans = i * fromIntegral (hsum1 + hsum2)
  print ("Answer = " ++ show ans)
  pure (i, hsum1 + hsum2)
  where
    printArmy ar = do
      print ar
      print $ "Hitpoints " ++ show (map hitPoints $ IM.elems ar)
      let hpsum = sum $ map hitPoints $ IM.elems ar
      print $ "Hitpoint sum " ++ show hpsum
      pure hpsum

goddammit :: Cave () -> IO (Int, HitPoints)
goddammit = uncurry (aaaaahhhhh 0) . updateCave
  where
    aaaaahhhhh completedRounds ges cv =
      let (fin, ges', cv') = oneRound (getPlayOrder ges) ges cv
      in if gameOver ges'
         then results (completedRounds + if fin then 1 else 0) ges' cv'
         else aaaaahhhhh (completedRounds + 1) ges' cv'

awwHellNaw :: Cave () -> IO (Word, (Int, HitPoints))
awwHellNaw cave = do
  ok <- reeeeeeeeee 1 60 combs icave
  writeIORef elfAttackPower (fromIntegral ok)
  (ok,) <$> goddammit cave
  where
    (combs, icave) = updateCave cave
    nelves = IM.size (elves combs)
    zeroElfDeaths ges cv =
      let (_, ges', cv') = oneRound (getPlayOrder ges) ges cv in if
        | IM.size (elves ges') /= nelves -> False
        | gameOver ges' -> True
        | otherwise -> zeroElfDeaths ges' cv'
    reeeeeeeeee lo hi combs icave
      | lo == hi = pure lo
      | otherwise = do
          let pow = (lo + hi) `div` 2
          writeIORef elfAttackPower (fromIntegral pow)
          if zeroElfDeaths combs icave then reeeeeeeeee lo pow combs icave
          else if pow == lo then pure hi
               else reeeeeeeeee pow hi combs icave

testcase1 :: String
testcase1 = "\
\#######\n\
\#.G...#\n\
\#...EG#\n\
\#.#.#G#\n\
\#..G#E#\n\
\#.....#\n\
\#######"

testcase2 :: String
testcase2 = "\
\#######\n\
\#G..#E#\n\
\#E#E.E#\n\
\#G.##.#\n\
\#...#E#\n\
\#...E.#\n\
\#######"

testcase3 :: String
testcase3 = "\
\#######\n\
\#E..EG#\n\
\#.#G.E#\n\
\#E.##E#\n\
\#G..#.#\n\
\#..E#.#\n\
\#######"

testcase4 :: String
testcase4 = "\
\#######\n\
\#E.G#.#\n\
\#.#G..#\n\
\#G.#.G#\n\
\#G..#.#\n\
\#...E.#\n\
\#######"

testcase5 :: String
testcase5 = "\
\#######\n\
\#.E...#\n\
\#.#..G#\n\
\#.###.#\n\
\#E#G#G#\n\
\#...#G#\n\
\#######"

testcase6 :: String
testcase6 = "\
\#########\n\
\#G......#\n\
\#.E.#...#\n\
\#..##..G#\n\
\#...##..#\n\
\#...#...#\n\
\#.G...G.#\n\
\#.....G.#\n\
\#########"

dec15 :: IO ()
dec15 = do
  inp <- readFile "data/dec15.txt"

  let lol x f i = do y <- f (justParse caveP i); assert (y == x) (pure ())
  lol (47, 590) goddammit testcase1
  lol (37, 982) goddammit testcase2
  lol (46, 859) goddammit testcase3
  lol (35, 793) goddammit testcase4
  lol (54, 536) goddammit testcase5
  lol (20, 937) goddammit testcase6

  let huh x f i = do y <- f (justParse caveP i); assert (fst y == x) (pure ())
  huh 15 awwHellNaw testcase1
  huh 4  awwHellNaw testcase3
  huh 15 awwHellNaw testcase4
  huh 12 awwHellNaw testcase5
  huh 34 awwHellNaw testcase6

  let cave = justParse caveP inp
  void $ goddammit cave
  print =<< awwHellNaw cave
