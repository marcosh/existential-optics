{-# LANGUAGE InstanceSigs #-}

module PowerSeries where

import Data.Bifunctor

data PowerSeries a x
  = Const a
  | Poly  (PowerSeries a x) x

instance Bifunctor PowerSeries where
  first :: (a -> b) -> PowerSeries a x -> PowerSeries b x
  first f (Const a)   = Const (f a)
  first f (Poly ps x) = Poly (first f ps) x

  second :: (x -> y) -> PowerSeries a x -> PowerSeries a y
  second f (Const a)   = Const a
  second f (Poly ps x) = Poly (second f ps) (f x)

actionProduct :: Semigroup x => (a -> b -> c) -> PowerSeries a x -> PowerSeries b x -> PowerSeries c x
actionProduct f (Const a)     (Const b)      = Const $ f a b
actionProduct f (Const a)     (Poly psbx x)  = Poly (first (f a) psbx) x
actionProduct f (Poly psax x) (Const b)      = Poly (first (flip f b) psax) x
actionProduct f (Poly psax x) (Poly psbx x') = Poly (actionProduct f psax psbx) (x <> x')
