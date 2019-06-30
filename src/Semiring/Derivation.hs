{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Semiring.Derivation (Derivation(..), mkDerivation, fromDerivation) where
import Semiring.Semiring
import qualified Data.Set as S 
import Data.Monoid
import Data.Maybe(isNothing)
import Data.List 

-- | The 'Derivation' semiring keeps track of a single path or derivation 
--   that led to the known output. If there are more than one path it discards 
--   in favor the lesser path (based on ord). 
--

--   Derivation takes a Monoid as an argument that describes how to build up paths or 
--   more complicated structures.  
newtype Derivation m = Derivation (Maybe m) deriving (Eq, Ord) 

instance (Monoid m,Ord m) => Semiring (Derivation m) where
    zero = Derivation Nothing  
    one = (Derivation .Just) mempty
    Derivation d1 <+> Derivation d2 = 
        Derivation $ max d1 d2   
    Derivation d1 <.> Derivation d2 = Derivation $ do 
        d1' <- d1
        d2' <- d2
        return $ mappend d1' d2'


instance (Show m) => Show (Derivation m) where 
    show (Derivation (Just m)) = show m 
    show (Derivation Nothing) = "[]" 

mkDerivation :: (Monoid m ) => m -> Derivation m 
mkDerivation = Derivation .Just  

fromDerivation :: (Monoid m ) => Derivation m -> m 
fromDerivation (Derivation (Just m)) = m  




