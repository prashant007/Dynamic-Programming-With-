module Explanation.Labeled where

data Labeled a = Label String a deriving Show 


unlabel :: Labeled a -> a
unlabel (Label _ x) = x


instance Eq a => Eq (Labeled a) where
  Label _ x == Label _ y = x == y 

instance Ord a => Ord (Labeled a) where
   Label _ x <= Label _ y = x <= y 

instance Functor Labeled where
  fmap f (Label s x) = Label s (f x) 


instance Applicative Labeled where
  pure = Label "" 
  Label s1 f <*> Label s2 x = Label s1 (f x)


instance Num a => Num (Labeled a) where
  Label s1 x + Label s2 y = Label s1 (x+y)
  Label s1 x * Label s2 y = Label s1 (x*y)
  Label s1 x - Label s2 y = Label s1 (x-y)
  negate = fmap negate
  abs    = fmap negate
  signum = fmap signum
  fromInteger x = Label "" (fromInteger x)