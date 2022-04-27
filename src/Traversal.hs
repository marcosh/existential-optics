{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Traversal where

import Morph
import Optics
import PowerSeries
import Vect

-- fin
import Data.Nat

-- DATA TYPE

type Traversal = Optic PowerSeries

-- COMMON TRAVERSALS

both :: Morph PowerSeries f => Optic f (a, a) (b, b) a b
both = morph bothTraversal
  where
    bothTraversal :: Traversal (a, a) (b, b) a b
    bothTraversal = Optic deconstruct reconstruct

    deconstruct :: (a, a) -> PowerSeries '( 'S ('S 'Z), ()) a
    deconstruct (a1, a2) = PowerSeries () (a1 :. a2 :. Nil)

    reconstruct :: PowerSeries '(S (S Z), ()) b -> (b, b)
    reconstruct (PowerSeries _ ps) = (ps ! 0, ps ! 1)

bothEither :: Morph PowerSeries f => Optic f (Either a a) (Either b b) a b
bothEither = morph bothEitherTraversal
  where
    bothEitherTraversal :: Traversal (Either a a) (Either b b) a b
    bothEitherTraversal = Optic deconstruct reconstruct

    deconstruct :: Either a a -> PowerSeries '( 'S 'Z, Bool) a
    deconstruct (Left  a) = PowerSeries True  (a :. Nil)
    deconstruct (Right a) = PowerSeries False (a :. Nil)

    reconstruct :: PowerSeries '( 'S 'Z, Bool) a -> Either a a
    reconstruct (PowerSeries True  v) = Left  (v ! 0)
    reconstruct (PowerSeries False v) = Right (v ! 0)

-- COMMON OPERATIONS

over :: Traversal s t a b -> (a -> b) -> s -> t
over (Optic g h) f = h . fmap f . g
