{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Optics where

import Affine
import Associativity
import ConstOp
import Morph
import PowerSeries
import TypeTuples

-- base
import Data.Bifunctor

-- GENERAL OPTICS

data Optic f s t a b = forall c. Optic (s -> f c a) (f c b -> t)

type Optic' f s a = Optic f s s a a

-- CHANGE OPTIC TYPE

morph :: Morph f g => Optic f s t a b -> Optic g s t a b
morph (Optic f g) = Optic (f2g . f) (g . g2f)

-- COMPOSE OPTICS

(%) :: (ExistentiallyAssociative f, forall x. Functor (f x)) => Optic f s t u v -> Optic f u v a b -> Optic f s t a b
(%) (Optic su vt) (Optic ua bv) = Optic (existentialAssociateL . fmap ua . su) (vt . fmap bv . existentialAssociateR)
