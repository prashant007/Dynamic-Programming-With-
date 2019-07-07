module Semiring.View where

import Semiring.Semiring
import Semiring.Large
import Semiring.MaxPlus

data View a b = V a b deriving Show 

class Selector a where
  first :: a -> a -> Bool

instance Ord a => Selector (Large a) where
  first = (<=)

instance Ord a => Selector (NegInf a) where
  first = (>)


instance (Selector a,Semiring a,Monoid b) => Semiring (View a b) where
  zero = V zero mempty
  one  = V one mempty
  l@(V x _) <+> r@(V y _) = if first x y then l else r
  (V x a)   <.> (V y b)   = V (x <.> y) (mappend a b)

data View1 a b = V1 a (Maybe b) deriving Show 

instance (Selector a,Semiring a,Monoid b) => Semiring (View1 a b) where
  zero = V1 zero Nothing
  one  = V1 one (Just mempty)
  l@(V1 x _) <+> r@(V1 y _) = if first x y then l else r
  (V1 x a)   <.> (V1 y b)   = V1 (x <.> y) (mappend a b)