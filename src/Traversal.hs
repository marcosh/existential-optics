module Traversal where

import Optics
import PowerSeries

-- DATA TYPE

type Traversal = Optic PowerSeries

-- instance ExistentiallyAssociative PowerSeries where
--   type E PowerSeries a b = PowerSeries a b
--   type C PowerSeries a b c = Semigroup c

--   existentialAssociateL :: Semigroup c => PowerSeries a (PowerSeries b c) -> PowerSeries (PowerSeries a b) c
--   existentialAssociateL (Const a)           = Const (Const a)
--   existentialAssociateL (Poly psapsbc psbc) = actionProduct (\ps b -> _wB) (existentialAssociateL psapsbc) psbc

--   existentialAssociateR :: PowerSeries (PowerSeries a b) c -> PowerSeries a (PowerSeries b c)
--   existentialAssociateR ps = _