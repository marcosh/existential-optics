{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module PowerSeries where

import TypeTuples

-- base
import GHC.TypeNats

-- fixed-vector
import Data.Vector.Fixed
import Data.Vector.Fixed.Boxed
import Data.Vector.Fixed.Cont

data PowerSeries c a where
  PowerSeries :: Arity (Fst c) => Snd c -> Vec (Fst c) a -> PowerSeries c a

instance Functor (PowerSeries c) where
  fmap :: (a -> b) -> PowerSeries c a -> PowerSeries c b
  fmap f (PowerSeries c v) = PowerSeries c (Data.Vector.Fixed.map f v)
