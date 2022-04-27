{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Traversal where

import Morph
import Optics
import PowerSeries

-- fixed-vector
import Data.Vector.Fixed
import Data.Vector.Fixed.Boxed

-- DATA TYPE

type Traversal = Optic PowerSeries

-- COMMON TRAVERSALS

both :: Morph PowerSeries f => Optic f (a, a) (b, b) a b
both = morph bothTraversal
  where
    bothTraversal :: Traversal (a, a) (b, b) a b
    bothTraversal = Optic deconstruct reconstruct

    deconstruct :: (a, a) -> PowerSeries '(2, ()) a
    deconstruct (a1, a2) = PowerSeries () (mk2 a1 a2)

    reconstruct :: PowerSeries '(2, ()) b -> (b, b)
    reconstruct (PowerSeries _ ps) = (ps ! 0, ps ! 1)

bothEither :: Morph PowerSeries f => Optic f (Either a a) (Either b b) a b
bothEither = morph bothEitherTraversal
  where
    bothEitherTraversal :: Traversal (Either a a) (Either b b) a b
    bothEitherTraversal = Optic deconstruct reconstruct

    deconstruct :: Either a a -> PowerSeries '(1, Bool) a
    deconstruct (Left  a) = PowerSeries True  (mk1 a)
    deconstruct (Right a) = PowerSeries False (mk1 a)

    reconstruct :: PowerSeries '(1, Bool) a -> Either a a
    reconstruct (PowerSeries True  v) = Left  (v ! 0)
    reconstruct (PowerSeries False v) = Right (v ! 0)

-- traversedList :: Morph PowerSeries f => Optic f [a] [b] a b
-- traversedList = morph traversedListTraversal
--   where
--     traversedListTraversal :: Traversal [a] [b] a b
--     traversedListTraversal = Optic deconstruct reconstruct

--     deconstruct :: [a] -> PowerSeries '(n, ()) a
--     deconstruct l = _

--     reconstruct :: PowerSeries '(n, ()) a -> [a]
--     reconstruct ps = _

-- COMMON OPERATIONS

over :: Traversal s t a b -> (a -> b) -> s -> t
over (Optic g h) f = h . fmap f . g
