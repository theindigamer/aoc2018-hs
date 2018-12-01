{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common
  ( module Common
  , module Text.Megaparsec
  , module Control.Monad
  , module Data.Coerce
  , module Data.String
  , module Data.Monoid
  , module Data.Foldable
  , module Data.List.NonEmpty
  , Void
  , Set
  , IntSet
  , HashMap
  , HashSet
  , Map
  )
  where

import Data.Void (Void)

import Control.Applicative (Alternative)
import Control.Monad
import Data.Coerce
import Data.String (IsString (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid
import Data.Foldable
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Set (Set)
import Data.IntSet (IntSet)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Map.Strict (Map)

type Parser s = Parsec Void s

type PE = ParseError Char Void

type MonadChar e s m = (MonadParsec e s m, Token s ~ Char)

space :: MonadChar e s m => m ()
space = L.space C.space1 empty empty

lexeme :: MonadChar e s m => m a -> m a
lexeme = L.lexeme space

brackets :: MonadChar e s m => Char -> Char -> m a -> m a
brackets c1 c2 = between (lexeme (C.char c1)) (lexeme (C.char c2))

braces, squares, parens :: MonadChar e s m => m a -> m a
braces = brackets '{' '}'
squares = brackets '[' ']'
parens = brackets '(' ')'

justParse :: (Show (Token s), Show e) => Parsec e s a -> s -> a
justParse p s = case parse p "" s of
  Left x -> error (show x)
  Right a -> a

readSignedNum :: (Num a, Read a) => String -> a
readSignedNum ('+':cs) = read cs
readSignedNum cs = read cs

-- | NOTE: Consumes space after the integer.
signedIntP :: (MonadChar e s m, Integral a) => m a
signedIntP = L.signed space L.decimal

-- | NOTE: Consumes space after the float.
signedFloatP :: (MonadChar e s m, RealFloat a) => m a
signedFloatP = L.signed space L.float

-- * From Elude

{-# INLINE (<&>) #-}
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

thenOnly :: Alternative f => Bool -> a -> f a
thenOnly False _ = empty
thenOnly True  a = pure a

-- Equivalent to 'Data.List.Split.chop' but doesn't require you to write a
-- partial function.
chop :: (NonEmpty a -> (b, [a])) -> [a] -> [b]
chop _ [] = []
chop f (x:xs) = b : chop f as'
  where (b, as') = f (x:|xs)

-- >>> spanWhile (<) 5 [8, 16, 12]
-- ([8, 16], [12])
--
-- Equivalent to 'Data.List.span' but allows access to the previous encountered
-- element while going through the list.
spanWhile :: (a -> a -> Bool) -> a -> [a] -> ([a], [a])
spanWhile = go id
  where
    go acc _ _ [] = (acc [], [])
    go acc f x (y:ys) = if f x y then go (acc . (y :)) f y ys
                        else (acc [], y:ys)

-- | Analogous to 'spanWhile' but takes the head of the list as the starting
-- argument, if available.
--
-- The head of the original list is now the head of the first returned list.
--
-- >>> spanWhile1 (<) [10, 20, 15]
-- ([10, 20], [15])
--
-- No exceptions are thrown if the input list is empty.
--
-- >>> spanWhile1 undefined []
-- ([], [])
spanWhile1 :: (a -> a -> Bool) -> [a] -> ([a], [a])
spanWhile1 _ []     = ([], [])
spanWhile1 f (x:xs) = let (ys, zs) = spanWhile f x xs in (x:ys, zs)

-- | Group neighboring elements in a list.
--
-- Equivalent to 'Data.List.groupBy' but returns 'NonEmpty' instead.
--
-- *NOTE:* Only neighboring elements are grouped. The list is NOT reordered.
--
-- >>> groupBy (==) [3, 4, 3]
-- [3 :| [], 4 :| [], 3 :| []]
--
--
-- *NOTE:* The grouping is done based off the /head/ of the resulting lists,
-- not using adjacent elements. Consequently if the first argument is not
-- symmetric, it may give surprising results. Use 'groupWhile' instead if you
-- want that behaviour.
--
-- >>> groupBy (<) [1, 3, 2]
-- [1 :| [3, 2]]
--
-- >>> groupWhile (<) [1, 3, 2]
-- [1 :| [3], 2]
groupBy :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
groupBy eq = chop (\(x :| xs) -> let (ys, zs) = span (eq x) xs in (x :| ys, zs))

groupWhile :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
groupWhile f = chop (\(x :| xs) -> let (ys, zs) = spanWhile f x xs in (x :| xs, ys))

foldCoerceMap
  :: forall m b a t. (Monoid m, Coercible b m, Coercible m b, Foldable t)
  => (a -> b) -> t a -> b
foldCoerceMap f = (coerce :: m -> b) . foldMap (coerce . f :: a -> m)

-- * From Sanna.Prelude

infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

infixl 4 >|>
(>|>) :: Functor f => f a -> (a -> b) -> f b
(>|>) = flip (<|<)

infixr 0 <|
(<|) :: (a -> b) -> a -> b
f <| x = f x

infixr 4 <|<
(<|<) :: Functor f => (a -> b) -> f a -> f b
(<|<) = (<$>)

infixl 9 .>
(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g . f

infixr 9 <.
(<.) :: (b -> c) -> (a -> b) -> a -> c
f <. g = f . g

infixr 8 <.:
(<.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(<.:) = (.) . (.)
