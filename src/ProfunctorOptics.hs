{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module ProfunctorOptics where

import Associativity
import Optics
import Unitality

import Data.Functor.Contravariant

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

class Profunctor p => Strong f p where
  hom :: p a b -> p (f c a) (f c b)

type ProOptic f s t a b = forall p. Strong f p => p a b -> p s t

existentialToProfunctor :: Optic f s t a b -> ProOptic f s t a b
existentialToProfunctor (Optic f g) = dimap f g . hom

newtype InsideOutOptic f a b s t = InsideOut { insideOut :: Optic f s t a b }

instance Profunctor (InsideOutOptic f a b) where
  dimap :: (r -> s) -> (t -> u) -> InsideOutOptic f a b s t -> InsideOutOptic f a b r u
  dimap rs tu (InsideOut (Optic g h)) = InsideOut (Optic (g . rs) (tu . h))

instance (forall x. Functor (f x), ExistentiallyAssociative f) => Strong f (InsideOutOptic f a b) where
  hom :: InsideOutOptic f a b s t -> InsideOutOptic f a b (f c s) (f c t)
  hom (InsideOut (Optic g h)) = InsideOut (Optic (existentialAssociateL . fmap g) (fmap h . existentialAssociateR))

profunctorToExistential :: (forall x. Functor (f x), ExistentiallyAssociative f, ExistentiallyUnital f) => ProOptic f s t a b -> Optic f s t a b
profunctorToExistential po = insideOut (po (InsideOut (Optic unitor unitorInv)))
