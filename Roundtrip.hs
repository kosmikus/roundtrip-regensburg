{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
module Roundtrip where

import Control.Category
import Data.Aeson  as A
import Data.Binary as B
import Data.Char
import Data.Monoid
import GHC.Generics
import Test.SmallCheck
import Test.SmallCheck.Series
import Text.Parsec hiding (runParser)
import qualified Text.Parsec.String as P
import Prelude hiding ((.), id)

---------------------------------------------------------------------------

-- Example 1: Binary
--
-- class Binary t where
--   put :: t -> Put
--   get :: Get t
--
-- decode :: Binary a => ByteString -> a
-- encode :: Binary a => a -> ByteString

---------------------------------------------------------------------------

-- Example 2: JSON
--
-- class FromJSON a where
--   parseJSON :: Value -> Parser a
--
-- class ToJSON a where
--   toJSON :: a -> Value
--
-- decode :: FromJSON a => ByteString -> Maybe a
-- encode :: ToJSON a => a -> ByteString

---------------------------------------------------------------------------

-- Example 3: Read / Show
--
-- class Read a where
--   readsPrec :: Int -> ReadS a
--
-- class Show a where
--   showsPrec :: Int -> a -> ShowS
--   show :: a -> String
--
-- read :: Read a => String -> a

---------------------------------------------------------------------------

data Direction = North | West | South | East
  deriving (Eq, Show, Read, Generic, Binary, ToJSON)

data Command = Go Direction | Look | Quit
  deriving (Eq, Show, Read, Generic, Binary, ToJSON)

roundtripReadShow :: (Eq a, Show a, Read a) => a -> Bool
roundtripReadShow x = read (show x) == x

roundtripBinary :: (Eq a, Binary a) => a -> Bool
roundtripBinary x = B.decode (B.encode x) == x

instance Monad m => Serial m Direction
instance Monad m => Serial m Command

testRoundtripReadShow :: IO ()
testRoundtripReadShow = smallCheck 4 (roundtripReadShow :: Command -> Bool)

testRoundtripBinary :: IO ()
testRoundtripBinary = smallCheck 4 (roundtripBinary :: Command -> Bool)

-- Parsers and renderers, individually

parseDirection :: P.Parser Direction
parseDirection =
      North <$ string "n"
  <|> West  <$ string "w"
  <|> South <$ string "s"
  <|> East  <$ string "e"

renderDirection :: Direction -> String
renderDirection North = "n"
renderDirection West  = "w"
renderDirection South = "s"
renderDirection East  = "e"

-- Abstracting from both parsing and printing

-- Parser:
--
-- String -> Maybe (a -> b, String)
--
-- Renderer:
--
-- b -> Maybe (String -> String, a)

data Parser   a b = Parser
  { runParser   :: String -> Maybe (a -> b, String) }
    -- corresponds to P.Parser (a -> b)
data Renderer a b = Renderer
  { runRenderer :: b -> Maybe (String -> String, a) }

pchar :: Char -> Parser s s
pchar c = Parser
  (\ input -> case input of
                (x : xs) | c == x -> Just (id, xs)
                _                 -> Nothing)

rchar :: Char -> Renderer s s
rchar c = Renderer
  (\ s -> Just ((c :), s))

dchar :: Char -> Duo s s
dchar c = Duo (pchar c) (rchar c)

ptest :: Parser a b -> String -> a -> Maybe b
ptest (Parser p) input a =
  case p input of
    Nothing         -> Nothing
    Just (f, _rest) -> Just (f a)

rtest :: Renderer a b -> b -> Maybe String
rtest (Renderer r) b =
  case r b of
    Nothing      -> Nothing
    Just (f, _a) -> Just (f "")

por :: Parser a b -> Parser a b -> Parser a b
por (Parser p1) (Parser p2) =
  Parser (\ input ->
    case p1 input of
      Nothing     -> p2 input
      Just result -> Just result)

ror :: Renderer a b -> Renderer a b -> Renderer a b
ror (Renderer r1) (Renderer r2) =
  Renderer (\ b ->
    case r1 b of
      Nothing     -> r2 b
      Just result -> Just result)

instance Monoid (Parser a b) where
  mempty :: Parser a b
  mempty = Parser (const Nothing)

  mappend :: Parser a b -> Parser a b -> Parser a b
  mappend = por

instance Monoid (Renderer a b) where
  mempty :: Renderer a b
  mempty = Renderer (const Nothing)

  mappend :: Renderer a b -> Renderer a b -> Renderer a b
  mappend = ror

-- Mapping over parsers

pmap :: (a -> b) -> Parser a b
pmap f = Parser (\ input -> Just (f, input))

-- Mapping over renderers

rmap :: (b -> Maybe a) -> Renderer a b
rmap f = Renderer (\ b -> case f b of
                            Nothing -> Nothing
                            Just a  -> Just (id, a))

-- Stacks as nested pairs.
-- We use '()' to denote the end.
data a :- b = a :- b
  deriving (Eq, Show)

infixr 5 :-

pop :: (a :- b) -> a
pop (a :- _b) = a

-- Parser corresponding to the North constructor
pNorth :: Parser s (Direction :- s)
pNorth = pmap (North :-)

-- Renderer corresponding to the North costructor
rNorth :: Renderer s (Direction :- s)
rNorth = rmap (\ (d :- s) -> do North <- return d; return s)

-- Constructor Duos can all be written in terms of dmap
dmap :: (a -> b) -> (b -> Maybe a) -> Duo a b
dmap f g = Duo (pmap f) (rmap g)

-- Constructor Duos for Direction and Command

dNorth, dSouth, dWest, dEast :: Duo s (Direction :- s)
dNorth = dmap (North :-) (\ (d :- s) -> do North <- return d; return s)
dSouth = dmap (South :-) (\ (d :- s) -> do South <- return d; return s)
dWest = dmap (West :-) (\ (d :- s) -> do West <- return d; return s)
dEast = dmap (East :-) (\ (d :- s) -> do East <- return d; return s)

dGo :: Duo (Direction :- s) (Command :- s)
dGo =
  dmap
    (\ (d :- s) -> Go d :- s)
    (\ (c :- s) -> do Go d <- return c; return (d :- s))

dLook :: Duo s (Command :- s)
dLook = dmap (Look :-) (\ (d :- s) -> do Look <- return d; return s)

dQuit :: Duo s (Command :- s)
dQuit = dmap (Quit :-) (\ (d :- s) -> do Quit <- return d; return s)

-- Sequencing

pidentity :: Parser a a
pidentity = Parser (\ input -> Just (id, input))

pcombine :: Parser b c -> Parser a b -> Parser a c
pcombine (Parser p1) (Parser p2) = Parser (\ input ->
  case p1 input of
    Nothing        -> Nothing
    Just (f, rest) -> case p2 rest of
                        Nothing              -> Nothing
                        Just (g, restOfRest) -> Just (f . g, restOfRest))

ridentity :: Renderer a a
ridentity = Renderer (\ a -> Just (id, a))

rcombine :: Renderer b c -> Renderer a b -> Renderer a c
rcombine (Renderer r1) (Renderer r2) = Renderer (\ c ->
  case r1 c of
    Nothing     -> Nothing
    Just (f, b) -> case r2 b of
                     Nothing     -> Nothing
                     Just (g, a) -> Just (f . g, a))

instance Category Parser where
  id  = pidentity
  (.) = pcombine

instance Category Renderer where
  id  = ridentity
  (.) = rcombine

-- Combined parsers and printers

data Duo a b = Duo (Parser a b) (Renderer a b)

instance Monoid (Duo a b) where
  mempty = Duo mempty mempty
  mappend (Duo p1 r1) (Duo p2 r2) = Duo (mappend p1 p2) (mappend r1 r2)

instance Category Duo where
  id = Duo id id
  Duo p1 r1 . Duo p2 r2 = Duo (p1 . p2) (r1 . r2)

-- Duos for directions and commands

dDirection :: Duo s (Direction :- s)
dDirection =
     dNorth . dchar 'n'
  <> dWest  . dchar 'w'
  <> dSouth . dchar 's'
  <> dEast  . dchar 'e'

dCommand :: Duo s (Command :- s)
dCommand =
     dGo   . dstring "go" . dReqSpace . dDirection
  <> dLook . dstring "look"
  <> dQuit . dstring "quit"

-- More combinators

dstring :: String -> Duo s s
dstring []       = id
dstring (x : xs) = dchar x . dstring xs

-- | Required spaces.
-- Parses arbitray whitespace, renders as a single space.
dReqSpace :: Duo s s
dReqSpace = Duo pspaces (rchar ' ')

pspaces :: Parser s s
pspaces = Parser $ \ input ->
  case input of
    (c : cs) | isSpace c -> Just (id, dropWhile isSpace cs)
    _                    -> Nothing

pdtest :: Duo a b -> String -> a -> Maybe b
pdtest (Duo p _r) = ptest p

rdtest :: Duo a b -> b -> Maybe String
rdtest (Duo _p r) = rtest r

pdtest1 :: Duo () (a :- ()) -> String -> Maybe a
pdtest1 d input = pop <$> pdtest d input ()

rdtest1 :: Duo () (a :- ()) -> a -> Maybe String
rdtest1 d a = rdtest d (a :- ())

roundtripDuo :: (Eq a) => Duo () (a :- ()) -> a -> Bool
roundtripDuo d x =
  case rdtest1 d x of
    Nothing -> False
    Just y  -> pdtest1 d y == Just x

testRoundtripDuo :: IO ()
testRoundtripDuo = smallCheck 4 (roundtripDuo dCommand)

-- Libraries to look at:
--
-- https://hackage.haskell.org/package/invertible-syntax
-- https://hackage.haskell.org/package/roundtrip
-- https://hackage.haskell.org/package/boomerang
-- https://hackage.haskell.org/package/JsonGrammar
--
-- ... and more
