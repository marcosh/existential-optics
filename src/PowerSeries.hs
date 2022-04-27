{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module PowerSeries where

import TypeTuples
import Vect

data PowerSeries c a = forall n. PowerSeries (Snd c) (Vect (Fst c) a)

instance Functor (PowerSeries c) where
  fmap :: (a -> b) -> PowerSeries c a -> PowerSeries c b
  fmap f (PowerSeries c v) = PowerSeries c (f <$> v)
