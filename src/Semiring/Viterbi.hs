{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Semiring.Viterbi where
import Semiring.Semiring
import Semiring.BestDerivations

newtype ViterbiProb = VProb Double
    deriving (Eq,Show,Num,Ord,Fractional) 

instance Semiring (ViterbiProb) where
    zero = 0
    one  = 1
    (<+>)  = max  
    (<.>)  = (*)  



