module Explanation.Decompose where

class FeatureType a where
  normalize :: a -> Double
  decompose :: a -> [(String,Double)]



 


