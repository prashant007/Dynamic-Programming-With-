{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
module Semiring.BestDerivations where
import Semiring.Semiring
import Data.List 

data Max10Best semi = Max10Best [semi] deriving (Eq, Show)

instance (Ord semi, Semiring semi) => Semiring (Max10Best semi) where
  zero = Max10Best []
  one  = Max10Best [one]
  Max10Best a <+> Max10Best b = 
    Max10Best .take 10.reverse.sort.nub $ a ++ b
  Max10Best a <.> Max10Best b = 
    Max10Best .take 10.reverse.sort.nub.map(uncurry (<.>)) $ cartesian a b 

data MaxBest semi = MaxBest semi deriving (Eq,Show,Ord)

-- instance Ord a => Ord (MaxBest a) where
--   MaxBest x <= MaxBest y = x <= y 

instance (Ord semi, Semiring semi) => Semiring (MaxBest semi) where
  zero = MaxBest zero 
  one  = MaxBest one
  m1 <+> m2 = max m1 m2 
  MaxBest a <.> MaxBest b = MaxBest $ a <.> b 


data Min10Best a = Min10Best [a] deriving Eq

instance Show a => Show (Min10Best a) where
  show (Min10Best x) = show x


instance (Ord semi, Semiring semi) => Semiring (Min10Best semi) where
  zero = Min10Best []
  one  = Min10Best [one]
  Min10Best a <+> Min10Best b = 
    Min10Best .take 10.sort.nub $ (a ++ b)
  Min10Best a <.> Min10Best b = 
    Min10Best .take 10.sort.nub.map (uncurry (<.>)) $ cartesian a b 


data MinBest a = MinBest a deriving (Eq,Ord)

instance Show a => Show (MinBest a) where
  show (MinBest x) = show x  

instance (Ord semi, Semiring semi) => Semiring (MinBest semi) where
  zero = MinBest zero 
  one  = MinBest one
  m1 <+> m2 = min m1 m2 
  MinBest a <.> MinBest b = MinBest $ a <.> b 



data AllDerivations semi = AllDerivations [semi] deriving (Eq,Show)

instance (Ord semi, Semiring semi) => Semiring (AllDerivations semi) where
  zero = AllDerivations []
  one  = AllDerivations [one]
  AllDerivations a <+> AllDerivations b = 
    AllDerivations .sort.nub $ a ++ b
  AllDerivations a <.> AllDerivations b = 
    AllDerivations .sort.nub.map (uncurry (<.>)) $ cartesian a b 


newtype MultiDerivation m = MultiDerivation [m]
    deriving (Eq, Show, Ord) 

instance Monoid m => Semiring (MultiDerivation m) where
  zero = MultiDerivation []
  one  = MultiDerivation [mempty]
  MultiDerivation s1 <+> MultiDerivation s2
    = MultiDerivation $ s1 ++ s2
  MultiDerivation d1 <.> MultiDerivation d2 
    = MultiDerivation .map (uncurry mappend) $ cartesian d1 d2

