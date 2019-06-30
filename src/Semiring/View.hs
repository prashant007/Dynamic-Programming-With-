module Semiring.View where

import Semiring.Semiring
import Semiring.Large

data View a b = V a b deriving Show 

class Selector a where
  first :: a -> a -> Bool

instance Ord a => Selector (Large a) where
  first = (<=)

instance (Selector a,Semiring a,Monoid b) => Semiring (View a b) where
  zero = V zero mempty
  one  = V one mempty
  l@(V x _) <+> r@(V y _) = if first x y then l else r
  (V x a)   <.> (V y b)   = V (x <.> y) (mappend a b)
