{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Roundtrip where

import Data.Aeson  as A
import Data.Binary as B

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

-- Datatypes
-- Roundtrips
-- Testing

-- Parsing
-- Printing

-- Abstracting from both parsing and printing

-- Stacks

-- Common operations

-- Combinators

