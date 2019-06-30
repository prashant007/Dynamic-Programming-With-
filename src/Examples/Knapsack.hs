module Examples.Knapsack where

import DPSolver.DP
import DPSolver.Solvers
import Semiring.Semiring

import Semiring.Derivation
import Semiring.BestDerivations
import Semiring.MaxPlus
import Data.List 

names1   = ["Shell","BMW","WHO","Lufthansa","PepsiCo","Tesla","Dow Chemicals"]
weights1,values1 :: [Double]
weights1 = [30,40,20,60,70,30,50]
values1  = [7 ,9 ,5 ,12,12,6 ,12]
values2  = [[10,2,0,-5],[7,4,0,-2],[1,0,4,0],[15,1,0,-4],[8,7,0,-3],[3,2,2,-1],[8,6,0,-2]]

weights3,values3 :: [Double]
names3   = ["A","B","C","D","E","F","G"]
values3  = [7,9,5,12,14,6,12]
weights3 = [3,4,2,6,7,3,5]

triple1:: [(String,Double,Double)]
triple1 = zip3 names1 weights1 values1
--triple2 = zip3 names1 weights1 values2

triple2 :: [(String,Double,Decompose Double)]
triple2 = map (\(x,y,z) -> (x,y,Decompose z)) (zip3 names1 weights1 values2)

-- ks :: [(String,Integer,Integer)] -> TopDownDP (Int,Integer) (NegInf Integer) 
-- ks triple (0,_) = one
-- ks triple (_,0) = one 
-- ks triple (k,b)  
--   |wk > b    = memoize(k-1,b) 
--   |otherwise = memoize(k-1,b) <+> ((constant.Fin')vk <.> memoize (k-1,b-wk)) 
--   where
--     tk@(nk,wk,vk) = triple !! (k-1)

-- knapSackrun :: [(String,Integer,Integer)] -> Integer -> (NegInf Integer) 
-- knapSackrun t b = runTopDownDP (length t,b) (ks t)

type Name   = String 
type Weight = Double 
type Value  = Double
type Ders   = Derivation [Name]
type BestDer = MaxBest(NegInf Double,Ders)
type BidList = [(Name,Weight,Value)]

--ks :: BidList -> DP (Int,Weight) BestDer
ks blist (0,_) = one
ks blist (_,0) = one 
ks blist (k,b)  
  |wk > b    = memoize(k-1,b) 
  |otherwise = memoize(k-1,b) <+> ((constant.semiBestDers) (nk,vk) <.> memoize (k-1,b-wk)) 
  where
    tk@(nk,wk,vk) = blist !! (k-1)

--semiBestDers :: (Name,Value) -> BestDer
semiBestDers (n,v) = MaxBest(Fin' v,mkDerivation [n])


--knapSackrun :: BidList -> Weight -> BestDer
knapSackrun t b = runDP (ks t) (length t,b)

--runknap  = knapSackrun triple1 150
runknap = knapSackrun triple2 150 
