{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Unitality where

class ExistentiallyUnital (f :: k -> * -> *) where
  type U f :: k

  unitor    :: f (U f) a -> a
  unitorInv :: a -> f (U f) a
