{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module TypeTuples where

type family Fst (a :: (k1, k2)) where
  Fst '(x, y) = x

type family Snd (a :: (k1, k2)) where
  Snd '(x, y) = y
