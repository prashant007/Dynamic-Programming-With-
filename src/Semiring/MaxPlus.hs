module Semiring.MaxPlus where 
import Semiring.Semiring
import Text.Printf

data NegInf a = NegInf | Fin' a deriving(Eq,Ord,Show) 

instance Functor NegInf where
  fmap f (Fin' d) = Fin' (f d)
  fmap _ NegInf   = NegInf

instance Applicative NegInf where
  pure = Fin'   
  Fin' f <*> m = fmap f m 
  _      <*> _ = NegInf


instance Num a => Num (NegInf a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  negate = fmap negate
  abs    = fmap negate
  signum = fmap signum
  fromInteger = Fin' .fromInteger

instance (Num a,Ord a) => Semiring (NegInf a) where
    zero = NegInf
    one  = Fin' 0
    m1 <+> m2 = max m1 m2  
    m1 <.> m2 = m1 + m2  



