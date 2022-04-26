{-# LANGUAGE ExistentialQuantification #-}

module PowerSeries where

import TypeTuples

-- fixed-vector
import Data.Vector.Fixed.Boxed

newtype PowerSeries a x = PowerSeries (Snd a, Vec (Fst a) x)
