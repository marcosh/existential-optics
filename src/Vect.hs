{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Vect where

-- base
import GHC.Exts

-- fin
import Data.Fin
import Data.Nat

data Vect n a where
  Nil :: Vect 'Z a
  (:.) :: a -> Vect n a -> Vect ('S n) a

infixr 5 :.

instance Functor (Vect n) where
  fmap :: (a -> b) -> Vect n a -> Vect n b
  fmap f Nil      = Nil
  fmap f (a :. v) = f a :. fmap f v

instance Foldable (Vect n) where
  foldr :: (a -> b -> b) -> b -> Vect n a -> b
  foldr _ b Nil      = b
  foldr f b (a :. v) = f a (foldr f b v)

(!) :: Vect n a -> Fin n -> a
(!) (a :. _) FZ     = a
(!) (a :. v) (FS k) = v ! k
