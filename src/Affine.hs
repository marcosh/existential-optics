{-# LANGUAGE DeriveFunctor #-}

module Affine where

import TypeTuples

newtype Affine a b = Affine (Either (Fst a) (Snd a, b))
  deriving Functor

-- possible alternative definition
--
-- data Affine a b where
--   Affine :: a ~ (a0, a1) => Either a0 (a1, b) -> Affine a b