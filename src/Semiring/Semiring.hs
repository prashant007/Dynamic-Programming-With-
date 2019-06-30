module Semiring.Semiring where

import Data.Function (on)

infixl 9 <.>
infixl 8 <+>

class Semiring a where 
  zero, one :: a 
  (<+>), (<.>) :: a -> a -> a 

cartesian as bs = [(a,b) | a <- as, b <- bs] 

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x = (<*>) (fmap f x)


-- | Dual semirings can be useful. For instance combining the  
--   Prob semiring and the MultiDerivation ring gives the total likelihood of 
--   a derivation along with the paths to get there. 
-- instance (Semiring a, Semiring b) => Semiring (a,b) where
--   zero = (zero,zero)
--   one  = (one,one)
--   (a1,b1) <+> (a2,b2) 
--       = (a1 <+> a2, b1 <+> b2)
--   (a1,b1) <.> (a2,b2) 
--       = (a1 <.> a2, b1 <.> b2)

sconcat :: Semiring a => [a] -> a
sconcat [] = zero 
sconcat (x:xs) = x <+> sconcat xs


-- ===============================================
-- ===============================================

pad :: Num a => Decompose a -> Decompose a -> a -> (Decompose a,Decompose a)
pad (Decompose xs) (Decompose ys) u = (Decompose xs1,Decompose ys1)
  where
    (xs1,ys1) = pad' xs ys u 


pad' :: Num a => [a] -> [a] -> a ->  ([a],[a])
pad' as bs u 
    | ld == 0   = (as,bs)
    | ld > 0    = (as,bs++zs)
    | otherwise = (as++zs,bs) 
  where
    zs    = replicate (absld) u 
    absld = abs ld 
    ld    = length as - length bs 


newtype Decompose a = Decompose [a]  deriving Show

-- instance Show a => Show (Decompose a) where
--   show (Decompose d) = show d  

sumList :: Num a => [a] -> a 
sumList [] = 0
sumList (x:xs) = x + sumList xs 


liftDecomp :: Num a => (a -> a -> a) -> a -> Decompose a -> Decompose a -> Decompose a 
liftDecomp f u x y = (<*>) (fmap f x1) y1
  where
    (x1,y1) = pad x y u 

instance (Eq a,Num a) => Eq (Decompose a) where
  Decompose xs == Decompose ys = sumList xs == sumList ys

instance (Ord a,Num a) => Ord (Decompose a) where
  Decompose xs <= Decompose ys = sumList xs <= sumList ys   

instance Functor Decompose where
  fmap f (Decompose d) = Decompose (map f d)

instance Applicative Decompose where
  pure x = Decompose [x]
  (<*>) (Decompose fs) (Decompose vs) 
      = Decompose $ zipWith (\f v -> f v) fs vs  

instance Num a => Num (Decompose a) where
  (+) = liftDecomp (+) 0 
  (*) = liftDecomp (*) 1 
  (-) = liftDecomp (-) 0
  negate = fmap negate
  abs    = fmap negate
  signum = fmap signum
  fromInteger x = Decompose $ [fromInteger x]


