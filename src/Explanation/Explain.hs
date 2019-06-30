module Explanation.Explain where

import qualified Data.Set as S 
import Data.List 
import Data.Function



type CompType = String 
type CompVal  = Double 

-- -- value difference calculation 
vd :: [(CompType,CompVal)] -> [(CompType,CompVal)] -> [(CompType,CompVal)]
vd = zipWith (\(c1,x) (c2,y) -> (c1,(x-y))) 

sortF :: [[a]] -> [[a]]
sortF =  sortBy (compare `on` length)

sumPairs :: Num a => [(b,a)] -> a 
sumPairs = foldr (\(_,q) acc -> acc + q) 0

powerList :: Ord a => [a] -> [[a]]
powerList = (map S.toList. S.toList. S.powerSet. S.fromList) 

mds :: [(CompType,CompVal)] -> [(CompType,CompVal)] -> [[(CompType,CompVal)]]
mds l1 l2 =  filter (\v -> length v == expL) expls 
  where
    -- calculate vd 
    vds  = vd l1 l2 
    -- find out negative elements in vd 
    nvds = filter (\y -> snd y < 0) vds 
    -- sum of negative elements in vd 
    nsum = (abs.sumPairs) nvds  
    -- positive elements in vd 
    pvds = vds \\ nvds  
    -- sorted subsets of all positive elements 
    subs = sortF $ powerList pvds \\ [[]]
    -- find subsets that are explanations 
    expls= filter (\p -> sumPairs p > nsum) subs
    -- length of smallest explanation
    expL = (length.head) expls

explanations :: [CompVal] -> [CompVal] -> [CompType] -> [[(CompType, CompVal)]]
explanations rv1 rv2 rts = mds (zip rts rv1) (zip rts rv2)

ex1,ex2 :: [CompVal]
ex1 = [20,4,4,2]
ex2 = [17,10,3,1]

rewNames :: [CompType]
rewNames = ["TravelTime","Traffic","Weather","Construction"]

