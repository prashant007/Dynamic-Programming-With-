module Semiring.AlternateDer where

import DPSolver.DP
import DPSolver.Solvers
import Semiring.Semiring

import Semiring.Derivation
import Semiring.BestDerivations
import Semiring.MinPlus
import Data.List 




class Selector a where
  first :: a -> a -> Bool

instance Ord a => Selector (Large a) where
  first = (<=)

data Surface a b = Surface a b deriving (Eq,Ord,Show)

instance (Selector a,Semiring a,Monoid b) => Semiring (Surface a b) where
  zero = Surface zero mempty
  one  = Surface one mempty
  s@(Surface x _) <+> t@(Surface y _) = if first x y then s else t
  Surface x a <.> Surface y b = Surface (x <.> y) (mappend a b)


instance (Selector a,Semiring a,Monoid b) => Semiring (a,b) where
  zero = (zero,mempty)
  one  = (one,mempty)
  l@(x,_) <+> r@(y,_) = if first x y then l else r
  (x,a)   <.> (y,b)   = (x <.> y,mappend a b)